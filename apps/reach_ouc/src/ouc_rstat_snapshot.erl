-module(ouc_rstat_snapshot).

-export([
	new/3,
	get/2,
	get_stat_bounds/2,
	get_stat_val/4,
	refresh/2,
	merge/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ouc_rstat.hrl").

-record(snap, {
	store,
	ts,
	ndxs,
	dt
}).

-define(DICT, dict).
-define(STATIC_EXPIRY, 15).

new(Store0, Ts, Ndxs) ->
	Conf = ouc_rstat_store:get(Store0, conf),
	RNames = ouc_rstat_conf:get_rule_names(Conf),

	{Store, Dt} = init_dt(RNames, Ts, Ndxs, Store0),

	Snap = #snap{
		store = Store,
		ts = Ts,
		ndxs = Ndxs,
		dt = Dt
	},
	{ok, Snap}.

get(#snap{store=Store}, store) ->
	Store;
get(#snap{ts=Ts}, ts) ->
	Ts;
get(#snap{ndxs=Ndx}, indices) ->
	Ndx;

get(#snap{store=Store}, rnames) ->
	Conf = ouc_rstat_store:get(Store, conf),
	ouc_rstat_conf:get_rule_names(Conf);

get(_, _) ->
	throw(badarg).

get_stat_bounds(#snap{store=Store, ts=Ts}, RName) ->
	%% could be better off memoized...
	Conf = ouc_rstat_store:get(Store, conf),
	{ok, R} = ouc_rstat_conf:get_rule(Conf, RName),
	ouc_rstat_rule:get_covered_bounds(R, Ts).

get_stat_val(#snap{dt=Dt}, RName, P, {join, Ndxs}) ->
	case ouc_rstat_prop:is_basic(P) of
		true ->
			read_joint(RName, P, Ndxs, Dt);
		_ ->
			ReadF = fun(Px) -> read_joint(RName, Px, Ndxs, Dt) end,
			ouc_rstat_prop:derive(P, ReadF)
	end;
get_stat_val(#snap{dt=Dt}, RName, P, Ndx) ->
	case ouc_rstat_prop:is_basic(P) of
		true ->
			case ?DICT:find({RName, P, Ndx}, Dt) of
				{ok, V} -> V;
				_ -> undefined
			end;
		_ ->
			ReadF = fun(Px) -> (catch ?DICT:fetch({RName, Px, Ndx}, Dt)) end,
			ouc_rstat_prop:derive(P, ReadF)
	end.

refresh(Snap, Opts) ->
	OldTs = Snap#snap.ts,
	OldDt = Snap#snap.dt,
	OldStore = Snap#snap.store,
	OldNdxs = Snap#snap.ndxs,

	Ts1 = proplists:get_value(ts, Opts, OldTs),
	Store1 = proplists:get_value(store, Opts, OldStore),
	Ndxs1 = proplists:get_value(indices, Opts, OldNdxs),

	%% Conf assumed identical to original Store
	Conf = ouc_rstat_store:get(Store1, conf),
	RNames = ouc_rstat_conf:get_rule_names(Conf),

	Props = ouc_rstat_prop:basic_props(),

	%% @todo - consider folding through dict instead if ndxs is unchanged

	GetOldVal = fun(K) ->
		case ?DICT:find(K, OldDt) of
			{ok, V} -> V;
			_ -> undefined
		end
	end,

	Keys = [{RName, P, Ndx} || RName <- RNames, P <- Props, Ndx <- Ndxs1],
	Reqs = [{P, Ndx, RName, GetOldVal(K)} || K={RName, P, Ndx} <- Keys],

	{ok, Store1, Vs} = ouc_rstat_store:multi_refresh(Store1, Ts1, OldTs,
		Reqs),

	Dt1 = ?DICT:from_list(lists:zip(Keys, Vs)),

	{ok, Snap#snap{store=Store1, ts=Ts1, ndxs=Ndxs1, dt=Dt1}}.

merge(SnapX, SnapY) ->
	%% under the assumption the conf and ts are the same for both

	NdxsX = SnapX#snap.ndxs,
	DtX = SnapX#snap.dt,

	NdxsY = SnapY#snap.ndxs,
	DtY = SnapY#snap.dt,

	NdxsM = lists:usort(NdxsX ++ NdxsY),
	DtM = ?DICT:merge(fun(_, V1, _) -> V1 end, DtX, DtY),

	{ok, SnapX#snap{ndxs=NdxsM, dt=DtM}}.

%% internal

init_dt(RNames, Ts, Ndxs, Store) ->
	Props = ouc_rstat_prop:basic_props(),
	Keys = [{RName, Prop, Ndx} || RName <- RNames, Prop <- Props, Ndx <- Ndxs],

	init_dt1(Keys, Ts, Store, dict:new()).

init_dt1([], _Ts, Store, Dt) ->
	{Store, Dt};
init_dt1([Key={RName, Prop, Ndx}|T], Ts, Store, Dt) ->
	{ok, Store1, V} =
		ouc_rstat_store:read_range(Store, Prop, Ts, Ndx, RName),
	Dt1 = dict:store(Key, V, Dt),
	init_dt1(T, Ts, Store1, Dt1).

read_joint(RName, P, Ndxs, Dt) ->
	Vals = [(catch ?DICT:fetch({RName, P, Ndx}, Dt)) || Ndx <- Ndxs],
	ouc_rstat_prop:join(P, Vals).

-ifdef(TEST).

-define(M, ?MODULE).

new_test_() ->
	{ok, Snap} = ?M:new(t_rstore(), tm(5), [[ndx1], [ndx2]]),

	[
		?_assertEqual(t_rstore(), ?M:get(Snap, store)),
		?_assertEqual(tm(5), ?M:get(Snap, ts)),
		?_assertEqual([[ndx1], [ndx2]], ?M:get(Snap, indices)),

		?_assertEqual([last_30m, last_15m], ?M:get(Snap, rnames)),

		?_assertThrow(badarg, ?M:get(Snap, something_else))
	].

get_stat_val_test_() ->
	{ok, Snap} = ?M:new(t_rstore(), tm(30), [[ndx1], [ndx2]]),
	[
		?_assertEqual(45,
			?M:get_stat_val(Snap, last_30m, ?PROP_TE_CALL_COUNT, [ndx1])),
		?_assertEqual(52,
			?M:get_stat_val(Snap, last_30m, ?PROP_ME_CALL_DURATION, [ndx2])),
		?_assertEqual(undefined,
			?M:get_stat_val(Snap, last_30m, ?PROP_ME_CALL_DURATION, [ndx3])),
		{"derived", ?_assertEqual(502/22,
			?M:get_stat_val(Snap, last_30m, ?PROP_AvE_CALL_DURATION, [ndx2]))},
		{"derived undef", ?_assertEqual(undefined,
			?M:get_stat_val(Snap, last_30m, ?PROP_AvE_CALL_DURATION, [ndx3]))},

		{"join", ?_assertEqual(45 + 22,
			?M:get_stat_val(Snap, last_30m, ?PROP_TE_CALL_COUNT,
				{join, [[ndx1], [ndx2]]}))},

		{"join undef", ?_assertEqual(undefined,
			?M:get_stat_val(Snap, last_30m, ?PROP_TE_CALL_COUNT,
				{join, [[ndx1], [ndx3]]}))},

		{"join derived", ?_assertEqual((1005 + 502)/(45 + 22),
			?M:get_stat_val(Snap, last_30m, ?PROP_AvE_CALL_DURATION,
				{join, [[ndx1], [ndx2]]}))},

		{"join derived undef", ?_assertEqual(undefined,
			?M:get_stat_val(Snap, last_30m, ?PROP_AvE_CALL_DURATION,
				{join, [[ndx1], [ndx3]]}))}
	].

get_stat_bounds_test() ->
	{ok, Snap} = ?M:new(t_rstore(), tm(), []),
	{From, To} = ?M:get_stat_bounds(Snap, last_30m),
	?assertEqual(tm(-30), From),
	?assertEqual(tm(0) - 1, To).

ts_update_snapshot_test() ->

	%% init
	Store1 = t_rstore(),
	{ok, Snap1} = ?M:new(Store1, tm(30), [[ndx1], [ndx2]]),

	?assertEqual(45,
		?M:get_stat_val(Snap1, last_30m, ?PROP_TE_CALL_COUNT, [ndx1])),

	%% update
	Store2 = t_update_rstore(Store1, [
		fun(Str) -> ouc_rstat_store:log(Str, tm(32),
			[[ndx1]], ?PROP_TE_CALL_COUNT, 25) end
	]),
	{ok, Snap2} = ?M:refresh(Snap1, [{store, Store2}, {ts, tm(35)}]),

	?assertEqual(Store2, ?M:get(Snap2, store)),
	?assertEqual(tm(35), ?M:get(Snap2, ts)),

	?assertEqual(50,
		?M:get_stat_val(Snap2, last_30m, ?PROP_TE_CALL_COUNT, [ndx1])).

ndx_update_snapshot_test() ->
	%% init
	Store1 = t_rstore(),
	{ok, Snap1} = ?M:new(Store1, tm(30), [[ndx2]]),

	?assertEqual(undefined,
		?M:get_stat_val(Snap1, last_30m, ?PROP_TE_CALL_COUNT, [ndx1])),

	{ok, Snap2} = ?M:refresh(Snap1, [{indices, [[ndx1]]}]),

	?assertEqual(45,
		?M:get_stat_val(Snap2, last_30m, ?PROP_TE_CALL_COUNT, [ndx1])).

merge_test_() ->
	Store1 = t_rstore(),

	{ok, SnapX} = ?M:new(Store1, tm(30), [[ndx1]]),
	{ok, SnapY} = ?M:new(Store1, tm(30), [[ndx2]]),

	{ok, SnapM} = ?M:merge(SnapX, SnapY),

	[?_assertEqual([[ndx1], [ndx2]], ?M:get(SnapM, indices)),

	?_assertEqual(45,
		?M:get_stat_val(SnapM, last_30m, ?PROP_TE_CALL_COUNT, [ndx1])),

	?_assertEqual(52,
		?M:get_stat_val(SnapM, last_30m, ?PROP_ME_CALL_DURATION, [ndx2]))].


t_rstore() ->
	{ok, Conf} = ouc_rstat_conf:new([{rules, [
			{dynamic, last_30m, {minute, 30}, 5},
			{dynamic, last_15m, {minute, 15}, 5}
		]}]),

	{ok, RStore} = ouc_rstat_store:new(ouc_rstat_store_dict, Conf, []),

	LogF = fun(Ts, Ndx, P, V) ->
		fun(Str) -> ouc_rstat_store:log(Str, Ts, [Ndx], P, V) end
	end,

	%% wish this were monadic...
	Fs = [
		LogF(tm(), [ndx1], ?PROP_TE_CALL_COUNT, 20),
		LogF(tm(), [ndx1], ?PROP_ME_CALL_DURATION, 50),
		LogF(tm(), [ndx1], ?PROP_TE_CALL_DURATION, 500),

		LogF(tm(5), [ndx1], ?PROP_TE_CALL_COUNT, 25),
		LogF(tm(5), [ndx1], ?PROP_ME_CALL_DURATION, 55),
		LogF(tm(5), [ndx1], ?PROP_TE_CALL_DURATION, 505),

		LogF(tm(), [ndx2], ?PROP_TE_CALL_COUNT, 22),
		LogF(tm(), [ndx2], ?PROP_ME_CALL_DURATION, 52),
		LogF(tm(), [ndx2], ?PROP_TE_CALL_DURATION, 502),

		LogF(tm(), [ndx3], ?PROP_TE_CALL_COUNT, 20),
		LogF(tm(), [ndx3], ?PROP_ME_CALL_DURATION, 50),
		LogF(tm(), [ndx3], ?PROP_TE_CALL_DURATION, 500)
	],

	t_update_rstore(RStore, Fs).

t_update_rstore(RStore, Fs) ->
	lists:foldl(fun(F, Acc) -> {ok, Acc1} = F(Acc), Acc1 end, RStore, Fs).

tm() -> tm(0).

tm(DeltaMin) ->
	Base = ouc_time:datetime_to_unixts({{2013, 05, 20}, {09, 00, 00}}),
	Base + (DeltaMin * 60).

-endif.