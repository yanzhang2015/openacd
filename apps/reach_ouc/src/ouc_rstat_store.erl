-module(ouc_rstat_store).

-include("ouc_rstat.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
	new/3,

	get/2,

	log/5,
	multi_log/4,
	log_range/5,

	read/5,
	read_range/5,
	refresh/7,
	multi_refresh/4
]).

-record(str, {
	mod :: atom(),
	conf,
	dt
}).

-type rstat_store_data() :: any().
% -type rstat_store_mod() :: atom().
-opaque rstat_store() :: #str{}.
-export_type([rstat_store/0]).

-callback init(any()) -> {ok, rstat_store_data()}.
-callback increment(rstat_store_data(), rstat_key(), number()) -> rstat_store_data().
-callback set_if_max(rstat_store_data(), rstat_key(), number()) -> rstat_store_data().
-callback read(rstat_store_data(), rstat_key()) -> {rstat_store_data(), number()}.
-callback needs_cache() -> boolean().

%% TODO -- create an ouc_rstat_store monad. (if only erlang had currying)

-spec new(atom(), conf(), any()) -> {ok, rstat_store()} | {error, any()}.
new(Mod, Conf, Props) ->
	case Mod:init(Props) of
		{ok, Dt} ->
			{ok, #str{mod=Mod, conf=Conf, dt=Dt}};
		Err ->
			Err
	end.

get(#str{mod=Mod}, mod) ->
	Mod;
get(#str{conf=Conf}, conf) ->
	Conf;
get(_, _) ->
	throw(badarg).

log(Str, Ts, Ndxs, P, Val) ->
	Intvls = get_update_intervals(Str, Ts),
	log_ndxs_intvls(Str, P, Val, Ndxs, Intvls).

multi_log(Str, Ts, Ndxs, PVs) ->
	Intvls = get_update_intervals(Str, Ts),

	Str1 = lists:foldl(fun({P, V}, StrX) ->
		{ok, StrY} = log_ndxs_intvls(StrX, P, V, Ndxs, Intvls),
		StrY
	end, Str, PVs),

	{ok, Str1}.

log_range(Str, Rg, Ndxs, P, F) ->
	Intvls = get_range_update_intervals(Str, Rg),
	log_ndxs_intvls(Str, P, F, Ndxs, Intvls).

read(#str{mod=Mod, conf=Conf, dt=Dt}=Str, {Op, _, _} = P, Ts, Ndx, RName) ->
	{ok, Rl} = ouc_rstat_conf:get_rule(Conf, RName),
	{_B, Intvls} = ouc_rstat_rule:get_covered_intervals(Rl, Ts),

	J = case Op of
		total -> fun(X, Y) -> X + Y end;
		max -> fun erlang:max/2
	end,

	ReadF = fun(Intvl, {DtX, Acc}) ->
		Key = {P, Intvl, Ndx},
		{DtY, V} = Mod:read(DtX, Key),
		{DtY, J(Acc, V)}
	end,

	{Dt1, V} = lists:foldl(ReadF, {Dt, 0}, Intvls),
	{ok, Str#str{dt=Dt1}, V}.


read_range(#str{mod=Mod, conf=Conf, dt=Dt}=Str, {_Op, _, _} = P, Ts, Ndx, RName) ->
	case erlang:function_exported(Mod, read_range, 2) of
		true ->
			{ok, Rl} = ouc_rstat_conf:get_rule(Conf, RName),
			{{MinFrom, MaxTo}, Intrvls} = ouc_rstat_rule:get_covered_intervals(Rl, Ts),
			[{From, To}|_] = Intrvls,
			Cov = To - From + 1,

			Key = {P, {MinFrom, MaxTo}, Cov, Ndx},
			{Dt1, V} = Mod:read_range(Dt, Key),
			{ok, Str#str{dt=Dt1}, V};
		false ->
			read(Str, P, Ts, Ndx, RName)
	end.

refresh(#str{mod=Mod, conf=Conf, dt=Dt}=Str, P, Ts, Ndx, RName,
		OldTs, OldV) when is_integer(OldV) ->
	{ok, Rl} = ouc_rstat_conf:get_rule(Conf, RName),
	Strat = ouc_rstat_rule:get_refresh_intervals(Rl, Ts, OldTs),

	%% @todo - should be lazy
	{B, CovIntvls} = ouc_rstat_rule:get_covered_intervals(Rl, Ts),

	{Dt1, V} = apply_refresh_strat(Strat, B, CovIntvls, OldV, Mod, P, Ndx, Dt),
	{ok, Str#str{dt=Dt1}, V};
refresh(Str, P, Ts, Ndx, RName, _OldTs, _OldV) ->
	read(Str, P, Ts, Ndx, RName).

multi_refresh(Str, Ts, OldTs, Reqs) ->
	#str{
		dt = Dt,
		conf = Conf,
		mod = Mod
	} = Str,

	Rules = ouc_rstat_conf:get_rules(Conf),

	StratCovs = dict:from_list([{ouc_rstat_rule:get_name(Rule),
		{ouc_rstat_rule:get_refresh_intervals(Rule, Ts, OldTs),
		ouc_rstat_rule:get_covered_intervals(Rule, Ts)}} ||
		Rule <- Rules]),

	{Dt1, Vs} = fold_refresh(Reqs, Mod, StratCovs, Dt, []),
	{ok, Str#str{dt=Dt1}, Vs}.

%% internal

get_update_intervals(#str{conf=Conf}, Ts) ->
	ouc_rstat_conf:get_unique_update_intervals(Conf, Ts, Ts).

get_range_update_intervals(#str{conf=Conf}, {_, Ts} = Rg) ->
	ouc_rstat_conf:get_unique_update_intervals_for_range(Conf, Rg, Ts).

log_ndxs_intvls(Str, P, Val, Ndxs, Intvls) when is_integer(Val) ->
	Fn = fun(_) -> Val end,
	log_ndxs_intvls(Str, P, Fn, Ndxs, Intvls);
log_ndxs_intvls(#str{mod=Mod, dt=Dt}=Str, {Op, _, _} = P,
		Fn, Ndxs, Intvls) when is_function(Fn, 1) ->
	F = case Op of
		total -> increment;
		max -> set_if_max
	end,

	WriteF = fun({Intvl, Ndx}, DtX) ->
		Key = {P, Intvl, Ndx},
		Val = Fn(Intvl),
		Mod:F(DtX, Key, Val)
	end,

	INs = [{Intvl, Ndx} || Intvl <- Intvls, Ndx <- Ndxs],
	Dt1 = lists:foldl(WriteF, Dt, INs),
	{ok, Str#str{dt=Dt1}}.

refresh_val1(total, OldV, ExpiredV, NewV) ->
	{ok, OldV + NewV - ExpiredV};

refresh_val1(max, OldV, _ExpiredV, NewV) when NewV >= OldV ->
	{ok, NewV};
refresh_val1(max, OldV, ExpiredV, _NewV) when OldV > ExpiredV ->
	{ok, OldV};
refresh_val1(max, _OldV, _ExpiredV, _NewV) ->
	error.

fold_refresh([], _Mod, _Strats, Dt, Acc) ->
	{Dt, lists:reverse(Acc)};
fold_refresh([{P, Ndx, RName, OldV}|T], Mod, Strats, Dt, Acc) ->
	{Strat, {B, CovIntvls}} = dict:fetch(RName, Strats),
	{Dt1, V} = apply_refresh_strat(Strat, B, CovIntvls, OldV, Mod, P, Ndx, Dt),
	fold_refresh(T, Mod, Strats, Dt1, [V|Acc]).

apply_refresh_strat({update, _, ExpiredIs, NewIs}, B, CovIntvls, OldV, Mod,
		{Op, _, _} = P, Ndx, Dt) when is_integer(OldV) ->
	J = case Op of
		total -> fun(X, Y) -> X + Y end;
		max -> fun erlang:max/2
	end,

	ReadF = fun(Intvl, {DtX, Acc}) ->
		Key = {P, Intvl, Ndx},
		{DtY, V} = Mod:read(DtX, Key),
		{DtY, J(Acc, V)}
	end,

	{Dt1, ExpiredV} = lists:foldl(ReadF, {Dt, 0}, ExpiredIs),
	{Dt2, NewV} = lists:foldl(ReadF, {Dt1, 0}, NewIs),

    {Bf, Bt} = B,

    %%lager:debug("refreshing result for ~s to ~s inclusive " ++
    %%"~p. Old: ~p, Expired: ~p, New: ~p",
    %%	[ouc_time:to_sstring(Bf), ouc_time:to_sstring(Bt),
    %%	{P, Ndx}, OldV, ExpiredV, NewV]),

	case refresh_val1(Op, OldV, ExpiredV, NewV) of
		{ok, V} when V >= 0->
			{Dt2, V};
		{ok, V} ->
			Res = {_, Corrected} = read_range0(B, CovIntvls, Mod, P, Ndx, Dt2),
			lager:warning("invalid refresh result for ~s to ~s inclusive " ++
				"~p. Old: ~p, Expired: ~p, New: ~p, Derived: ~p, Corrected: ~p",
				[ouc_time:to_sstring(Bf), ouc_time:to_sstring(Bt),
				{P, Ndx}, OldV, ExpiredV, NewV, V, Corrected]),
			Res;
		_ ->
			read_range0(B, CovIntvls, Mod, P, Ndx, Dt2)
	end;
apply_refresh_strat({set, _, [Intvl]}, _B, _CovIntvls, _OldV,
		Mod, P, Ndx, Dt) ->
	Key = {P, Intvl, Ndx},
	Mod:read(Dt, Key);
apply_refresh_strat(_, B, CovIntvls, _OldV, Mod, P, Ndx, Dt) ->
	read_range0(B, CovIntvls, Mod, P, Ndx, Dt).

read_range0({MinFrom, MaxTo}, Intvls, Mod, P, Ndx, Dt) ->
	case erlang:function_exported(Mod, read_range, 2) of
		true ->
			[{From, To}|_] = Intvls,
			Cov = To - From + 1,

			Key = {P, {MinFrom, MaxTo}, Cov, Ndx},
			%%lager:debug("Calling repair read_range for key: ~p", [Key]),
			Mod:read_range(Dt, Key);
		false ->
			read0(Intvls, Mod, P, Ndx, Dt)
	end.

read0(Intvls, Mod, {Op, _, _} = P, Ndx, Dt) ->
	J = case Op of
		total -> fun(X, Y) -> X + Y end;
		max -> fun erlang:max/2
	end,

	ReadF = fun(Intvl, {DtX, Acc}) ->
		Key = {P, Intvl, Ndx},
		{DtY, V} = Mod:read(DtX, Key),
		{DtY, J(Acc, V)}
	end,
	lists:foldl(ReadF, {Dt, 0}, Intvls).

-ifdef(TEST).

-define(M, ?MODULE).

new_test_() ->
	{ok, RStore} = ?M:new(ouc_rstat_store_dict, t_conf(), []),

	[
		?_assertEqual(ouc_rstat_store_dict, ?M:get(RStore, mod)),
		?_assertEqual(t_conf(), ?M:get(RStore, conf)),

		?_assertThrow(badarg, ?M:get(RStore, something_else))
	].

log_test_() ->
	{ok, RStore} = ?M:new(ouc_rstat_store_dict, t_conf(), []),

	[{"accumulating - total", fun() ->
		{ok, RStore1} = ?M:log(RStore, tm(), [[]], ?PROP_TE_CALL_COUNT, 5),
		{ok, RStore2} = ?M:log(RStore1, tm(), [[]], ?PROP_TE_CALL_COUNT, 10),
		{ok, RStore3} = ?M:log(RStore2, tm(), [[]], ?PROP_TE_CALL_COUNT, 20),

		?assertMatch({ok, _, 35},
			?M:read(RStore3, ?PROP_TE_CALL_COUNT, tm(5), [], last_30m))
	end}, {"accumulating - max", fun() ->
		{ok, RStore1} = ?M:log(RStore, tm(), [[]], ?PROP_ME_CALL_DURATION, 5),
		{ok, RStore2} = ?M:log(RStore1, tm(), [[]], ?PROP_ME_CALL_DURATION, 20),
		{ok, RStore3} = ?M:log(RStore2, tm(), [[]], ?PROP_ME_CALL_DURATION, 10),

		?assertMatch({ok, _, 20},
			?M:read(RStore3, ?PROP_ME_CALL_DURATION, tm(5), [], last_30m))
	end}, {"diff interval - total", fun() ->
		{ok, RStore1} = ?M:log(RStore, tm(0), [[]], ?PROP_TE_CALL_COUNT, 5),
		{ok, RStore2} = ?M:log(RStore1, tm(8), [[]], ?PROP_TE_CALL_COUNT, 10),
		{ok, RStore3} = ?M:log(RStore2, tm(9), [[]], ?PROP_TE_CALL_COUNT, 12),

		?assertMatch({ok, _, 22},
			?M:read(RStore3, ?PROP_TE_CALL_COUNT, tm(35), [], last_30m))
	end}, {"diff interval - max", fun() ->
		{ok, RStore1} = ?M:log(RStore, tm(0), [[]], ?PROP_ME_CALL_DURATION, 5),
		{ok, RStore2} = ?M:log(RStore1, tm(8), [[]],
			?PROP_ME_CALL_DURATION, 12),
		{ok, RStore3} = ?M:log(RStore2, tm(9), [[]],
			?PROP_ME_CALL_DURATION, 10),

		?assertMatch({ok, _, 12},
			?M:read(RStore3, ?PROP_ME_CALL_DURATION, tm(35), [], last_30m))
	end},

	%% indices
	{"diff indices", fun() ->
		{ok, RStore1} = ?M:log(RStore, tm(), [[a]], ?PROP_TE_CALL_COUNT, 5),
		{ok, RStore2} = ?M:log(RStore1, tm(), [[b]], ?PROP_TE_CALL_COUNT, 10),
		?assertMatch({ok, _, 5},
			?M:read(RStore2, ?PROP_TE_CALL_COUNT, tm(5) - 5,
				[a], last_30m))
	end}, {"multi indices", fun() ->
		{ok, RStore1} = ?M:log(RStore, tm(), [[a], [b]],
			?PROP_TE_CALL_COUNT, 5),

		?assertMatch({ok, _, 5},
			?M:read(RStore1, ?PROP_TE_CALL_COUNT, tm(5) - 5,
				[a], last_30m)),
		?assertMatch({ok, _, 5},
			?M:read(RStore1, ?PROP_TE_CALL_COUNT, tm(5) - 5,
				[b], last_30m))
	end}
	].

multi_log_test() ->
	{ok, RStore} = ?M:new(ouc_rstat_store_dict, t_conf(), []),

	{ok, RStore1} = ?M:multi_log(RStore, tm(), [[a], [b]],
		[{?PROP_TE_CALL_COUNT, 5},
		{?PROP_ME_CALL_DURATION, 10}]),

	?assertMatch({ok, _, 5},
			?M:read(RStore1, ?PROP_TE_CALL_COUNT, tm(5), [a], last_30m)),
	?assertMatch({ok, _, 5},
			?M:read(RStore1, ?PROP_TE_CALL_COUNT, tm(5), [b], last_30m)),

	?assertMatch({ok, _, 10},
			?M:read(RStore1, ?PROP_ME_CALL_DURATION, tm(5), [a], last_30m)),
	?assertMatch({ok, _, 10},
			?M:read(RStore1, ?PROP_ME_CALL_DURATION, tm(5), [b], last_30m)).

log_range_test() ->
	{ok, RStore} = ?M:new(ouc_rstat_store_dict, t_conf(), []),

	{ok, RStore1} = ?M:log_range(RStore, {tm(0), tm(0) + 12}, [[a]],
		?PROP_TS_IDLE_DURATION, fun({F, T}) ->
			T - F + 1
		end),

	?assertMatch({ok, _, 15},
		?M:read(RStore1, ?PROP_TS_IDLE_DURATION, tm(0) + 15, [a], last_30m)),

	?assertMatch({ok, _, 30 * 60},
		?M:read(RStore1, ?PROP_TS_IDLE_DURATION, tm(0) + 15, [a], this_30m)).

refresh_total_test() ->
	{ok, RStore0} = ?M:new(ouc_rstat_store_dict, t_conf(), []),
	{ok, RStore1} = ?M:log(RStore0, tm(0), [[a]], ?PROP_TE_CALL_COUNT, 5),
	{ok, RStore2} = ?M:log(RStore1, tm(0) + 5, [[a]], ?PROP_TE_CALL_COUNT, 3),
	{ok, RStore3} = ?M:log(RStore2, tm(30), [[a]], ?PROP_TE_CALL_COUNT, 7),
	{ok, RStore4} = ?M:log(RStore3, tm(30) + 5, [[a]], ?PROP_TE_CALL_COUNT, 9),

	OldVal = 90,
	OldTs = tm(30),
	NewTs = tm(30) + 10,

	?assertMatch({ok, _, 90 - (5 + 3) + (7 + 9)},
		?M:refresh(RStore4, ?PROP_TE_CALL_COUNT, NewTs, [a],
			last_30m, OldTs, OldVal)).

refresh_max_test_() ->
	{ok, RStore0} = ?M:new(ouc_rstat_store_dict, t_conf(), []),
	{ok, RStore1} = ?M:log(RStore0, tm(0), [[a]], ?PROP_ME_CALL_DURATION, 10),
	{ok, RStore2} = ?M:log(RStore1, tm(5), [[a]], ?PROP_ME_CALL_DURATION, 9),

	OldTs = tm(30),
	NewTs = tm(30) + 5,

	GetRVal = fun(OldVal, NewVal) ->
		{ok, RStore3} = ?M:log(RStore2, tm(30),	[[a]],
			?PROP_ME_CALL_DURATION, NewVal),
		{ok, _, V} = ?M:refresh(RStore3, ?PROP_ME_CALL_DURATION, NewTs, [a],
			last_30m, OldTs, OldVal),
		V
	end,

	[
		{"old >= new > expired", ?_assertMatch(20, GetRVal(20, 3))},
		{"new > old", ?_assertMatch(25, GetRVal(20, 25))},
		{"expired = old AND expired > new", ?_assertMatch(9, GetRVal(5, 3))}
	].

refresh_static_rule_test() ->
	{ok, RStore0} = ?M:new(ouc_rstat_store_dict, t_conf(), []),
	{ok, RStore1} = ?M:log(RStore0, tm(10), [[a]], ?PROP_TE_CALL_DURATION, 100),

	?assertMatch({ok, _, 100},
		?M:refresh(RStore1, ?PROP_TE_CALL_DURATION, tm(25), [a], this_30m,
			tm(20), 5)).

refresh_read_fallback_test() ->
	{ok, RStore0} = ?M:new(ouc_rstat_store_dict, t_conf(), []),
	{ok, RStore1} = ?M:log(RStore0, tm(0), [[a]], ?PROP_TE_CALL_COUNT, 5),

	OldVal = undefined,

	?assertMatch({ok, _, 5},
		?M:refresh(RStore1, ?PROP_TE_CALL_COUNT, tm(5), [a],
			last_30m, tm(0), OldVal)).

invalid_refresh_test() ->
	{ok, RStore0} = ?M:new(ouc_rstat_store_dict, t_conf(), []),
	{ok, RStore1} = ?M:log(RStore0, tm(0), [[a]], ?PROP_TE_CALL_COUNT, 5),
	{ok, RStore2} = ?M:log(RStore1, tm(5), [[a]], ?PROP_TE_CALL_COUNT, 10),

	OldVal = 4, %% should be 15, but say there was an error

	?assertMatch({ok, _, 10},
		?M:refresh(RStore2, ?PROP_TE_CALL_COUNT, tm(30) + 5, [a],
			last_30m, tm(30), OldVal)).

multi_refresh_test() ->
	{ok, RStore0} = ?M:new(ouc_rstat_store_dict, t_conf(), []),
	{ok, RStore1} = ?M:log(RStore0, tm(0), [[a]], ?PROP_TE_CALL_DURATION, 10),
	{ok, RStore2} = ?M:log(RStore1, tm(0), [[a]], ?PROP_TE_CALL_COUNT, 3),

	{ok, RStore3} = ?M:log(RStore2, tm(-30), [[a]], ?PROP_ME_CALL_DURATION, 10),
	{ok, RStore4} = ?M:log(RStore3, tm(-10), [[a]], ?PROP_ME_CALL_DURATION, 8),

	{ok, RStore5} = ?M:log(RStore4, tm(-20), [[a]], ?PROP_TE_WAIT_DURATION, 10),
	{ok, RStore6} = ?M:log(RStore5, tm(-10), [[a]], ?PROP_TE_WAIT_DURATION, 20),

	?assertMatch(
		{ok, _, [110, 5, 8, 30]},
		?M:multi_refresh(RStore6, tm(0) + 5, tm(0),
			[{?PROP_TE_CALL_DURATION, [a], last_30m, 100},
			{?PROP_TE_CALL_COUNT, [a], last_30m, 2},
			{?PROP_ME_CALL_DURATION, [a], last_30m, 10},
			{?PROP_TE_WAIT_DURATION, [a], last_30m, undefined}
			])).

%% internal

t_conf() ->
	{ok, Conf} = ouc_rstat_conf:new([{rules, [
			{dynamic, last_30m, {minute, 30}, 5},
			{dynamic, last_15m, {minute, 15}, 5},
			{static, this_30m, {minute, 30}}
		]}]),
	Conf.

tm() -> tm(0).

tm(DeltaMin) ->
	Base = ouc_time:datetime_to_unixts({{2013, 05, 20}, {09, 00, 00}}),
	Base + (DeltaMin * 60).


-endif.

% log_call(St, Conf, Transactions0, Ndxs, Now) ->
% 	Transactions = clean_trans(Transactions0),

% 	St1 = log_call_end_props(St, Conf, Transactions, Ndxs, Now),
% 	log_call_inc_props(St1, Conf, Transactions, Ndxs, Now).

% %% @deprecated
% log_availability(St, Conf, Start, End, Ndxs, Now) ->
% 	log_agent_cstate(St, Conf, idle, Start, End, Ndxs, Now).

% log_agent_cstate(St, _Conf, _ACState, S, S, _Ndxs, _Now) ->
% 	%% If availability ended within the same second,
% 	%% that is, availability duration is < 1 sec,
% 	%% session is disregarded
% 	St;
% log_agent_cstate(St, Conf, ACState, Start, End, Ndxs, Now) ->
% 	End1 = End - 1,

% 	Intvls = ouc_rstat_conf:get_unique_update_intervals_for_range(Conf, {Start, End1}, Now),
% 	R = {Start, End1},

% 	P = get_prop_for_acstate(ACState),

% 	INAs = [{Intvl, Ndx, {P, get_seconds_intersection(R, Intvl)}} ||
% 		Intvl <- Intvls, Ndx <- Ndxs],
% 	store_data(St, INAs).

% take_snapshot(St, Conf, I, Ndx) ->
% 	ouc_rstat_snapshot:init(St, Conf, I, Ndx).

% -spec get_props(rstat_store(), conf(), instant(), rstat_cindex()) ->
% 	{rstat_store(), [{rule_name(), interval(), [{rstat_prop(), number()}]}]}.
% get_props(St, Conf, I, Ndx) ->
% 	get_props(St, Conf, all, I, Ndx).

% -spec get_props(rstat_store(), conf(), [rstat_prop()] | all, instant(), rstat_cindex()) ->
% 	{rstat_store(), [{rule_name(), interval(), [{rstat_prop(), number()}]}]}.
% get_props(St, Conf, Ps, I, Ndx) ->
% 	Rs = ouc_rstat_conf:get_rules(Conf),
% 	get_props_for_rules1(St, Ps, Rs, I, Ndx).


% get_props_for_rules(St, Conf, RNames, I, Ndx) ->
% 	get_props_for_rules(St, Conf, all, RNames, I, Ndx).

% -spec get_props_for_rules(rstat_store(), conf(), [rstat_prop()] | all, [rule_name()], instant(), rstat_cindex()) ->
% 	{rstat_store(), [{rule_name(), interval(), [{rstat_prop(), number()}]}]}.
% get_props_for_rules(St, Conf, Ps, RNames, I, Ndx) ->
% 	Rs = [R || R <- ouc_rstat_conf:get_rules(Conf), lists:member(ouc_rstat_rule:get_name(R), RNames)],
% 	get_props_for_rules1(St, Ps, Rs, I, Ndx).

% get_props_for_rule(St, Conf, RName, I, Ndx) ->
% 	get_props_for_rule(St, Conf, all, RName, I, Ndx).

% -spec get_props_for_rule(rstat_store(), conf(), [rstat_prop()] | all, instant(), rstat_cindex()) ->
% 	{ok, rstat_store(), {interval(), [{rstat_prop(), number()}]}} | {error, rule_not_found}.
% get_props_for_rule(St, Conf, Ps, RName, I, Ndx) ->
% 	case ouc_rstat_conf:get_rule(Conf, RName) of
% 		{ok, R} ->
% 			{B, Intvls} = ouc_rstat_rule:get_covered_intervals(R, I),
% 			{St2, V} = get_props_for_intervals(St, Ps, Intvls, Ndx),
% 			{ok, St2, {B, V}};
% 		_ ->
% 			{error, rule_not_found}
% 	end.

% basic_prop_keys() ->
% 	[
% 		?PROP_TE_CALL_COUNT,
% 		?PROP_TE_ABANDON_COUNT,
% 		?PROP_TE_ABANDON_DURATION,
% 		?PROP_TE_CALL_DURATION,
% 		?PROP_TE_RING_DURATION,
% 		?PROP_TE_WAIT_DURATION,
% 		?PROP_ME_CALL_DURATION,
% 		?PROP_ME_WAIT_DURATION,
% 		?PROP_TS_CALL_DURATION,

% 		?PROP_TS_RELEASED_DURATION,
% 		?PROP_TS_IDLE_DURATION,
% 		?PROP_TS_RINGING_DURATION,
% 		?PROP_TS_ONCALL_DURATION,
% 		?PROP_TS_WRAPUP_DURATION
% 	].

% extended_prop_keys() ->
% 	[
% 		?PROP_AvE_CALL_DURATION,
% 		?PROP_AvE_ABANDON_DURATION,
% 		?PROP_AvE_RING_DURATION,
% 		?PROP_AvE_WAIT_DURATION,
% 		?PROP_PcS_OCCUPANCY
% 	].

% %% debug helpers

% print_rstat_breakdown(St, Conf, P, RName, Ndx, Now) ->
% 	case ouc_rstat_conf:get_rule(Conf, RName) of
% 		{ok, R} ->
% 			{{F, T}, Intvls} = ouc_rstat_rule:get_covered_intervals(R, Now),
% 			io:format("Rule: ~p~n", [RName]),
% 			io:format("Now: ~s~nFrom: ~s~nTo: ~s~n", [
% 				ouc_time:to_sstring(Now), ouc_time:to_sstring(F), ouc_time:to_sstring(T)]),
% 			io:format("Property: ~p~n", [P]),

% 			io:format("-----~n"),

% 			lists:foreach(fun({If, It}=Intvl) ->
% 				K = {Intvl, P, Ndx},
% 				{_, V} = read(St, K),
% 				io:format("~s to ~s -- ~p~n", [ouc_time:to_sstring(If), ouc_time:to_sstring(It), V])
% 			end, Intvls),
% 			io:format("-----~n"),

% 			{ok, _, {_, Vz}} = get_props_for_rule(St, Conf, [P], RName, Now, Ndx),
% 			io:format("TOTAL: ~p~n", [proplists:get_value(P, Vz)]);
% 		_ ->
% 			io:format("Rule ~p not found~n", [RName])
% 	end.

% %% private
% get_prop_join_fun(P) ->
% 	case get_prop_join_action(P) of
% 		set_if_max -> fun erlang:max/2;
% 		_ -> fun(X, Y) -> X + Y end
% 	end.

% get_prop_join_action({max, _, _}) -> set_if_max;
% get_prop_join_action({total, _, _}) -> increment;
% get_prop_join_action(_) -> throw(badarg).

% %% internal

% log_call_end_props(St, Conf, Transactions, Ndxs, Now) ->
% 	InitTime = get_tran_start(Transactions),
% 	OncallInterval = get_oncall_interval(Transactions),

% 	case OncallInterval of
% 		{OncallStart, OncallEnd} ->
% 			RingingTime = get_ring_start(Transactions),

% 			Intervals = ouc_rstat_conf:get_unique_update_intervals(Conf, OncallEnd, Now),
% 			WaitDuration = OncallStart - InitTime,
% 			AnsDuration = case RingingTime of
% 				undefined -> 0;
% 				_ -> max(OncallStart - RingingTime, 0)
% 			end,

% 			OncallDuration = OncallEnd - OncallStart,

% 			Actions = [
% 				{?PROP_TE_CALL_COUNT, 1},
% 				{?PROP_TE_CALL_DURATION, OncallDuration},
% 				{?PROP_TE_RING_DURATION, AnsDuration},
% 				{?PROP_TE_WAIT_DURATION, WaitDuration},
% 				{?PROP_ME_CALL_DURATION, OncallDuration},
% 				{?PROP_ME_WAIT_DURATION, WaitDuration}
% 			],

% 			INAs = [{I, Ndx, A} || I <- Intervals, A <- Actions, Ndx <- Ndxs],
% 			store_data(St, INAs);
% 		_ ->
% 			EndTime = get_tran_end(Transactions),
% 			Intervals = ouc_rstat_conf:get_unique_update_intervals(Conf, EndTime, Now),

% 			AbandonTime = case (InitTime =:= undefined) orelse (EndTime =:= undefined) of
% 				true -> 0;
% 				_ -> max(EndTime - InitTime, 0)
% 			end,

% 			Actions = [
% 				{?PROP_TE_ABANDON_COUNT, 1},
% 				{?PROP_TE_ABANDON_DURATION, AbandonTime}
% 			],

% 			INAs = [{I, Ndx, A} || I <- Intervals, A <- Actions, Ndx <- Ndxs],
% 			store_data(St, INAs)
% 	end.

% log_call_inc_props(St, Conf, Transactions, Ndxs, Now) ->
% 	OncallInterval = get_oncall_interval(Transactions),

% 	case OncallInterval of
% 		{Start, End} when End > Start -> %% at least 1s
% 			End1 = End - 1,
% 			Intvls = ouc_rstat_conf:get_unique_update_intervals_for_range(Conf, {Start, End1}, Now),
% 			R = {Start, End1},

% 			INAs = [{Intvl, Ndx, {?PROP_TS_CALL_DURATION, get_seconds_intersection(R, Intvl)}} ||
% 				Intvl <- Intvls, Ndx <- Ndxs],
% 			store_data(St, INAs);
% 		_ ->
% 			St
% 	end.


% clean_trans(Trans0) ->
% 	Trans1 = [{State, as_cpx_time(Time)} || {State, Time} <- Trans0],
% 	lists:keysort(2, Trans1).

% as_cpx_time(Secs) when is_integer(Secs) -> Secs;
% as_cpx_time(Now) -> ouc_time:now(Now).

% get_tran_start([]) ->
% 	undefined;
% get_tran_start([{_, Time}|_]) ->
% 	Time.

% %% gets the last ring start
% %% WARNING: won't work right if transfers are present
% get_ring_start(Trans) ->
% 	proplists:get_value(ringing, lists:reverse(Trans)).

% get_oncall_interval([]) -> undefined;
% get_oncall_interval([{oncall, Start}, {_, End}|_]) -> {Start, End};
% get_oncall_interval([_|T]) -> get_oncall_interval(T).

% get_tran_end([]) ->
% 	undefined;
% get_tran_end(Trans) ->
% 	{_, Time} = lists:last(Trans),
% 	Time.


% get_props_for_rules1(St, Ps, Rs, I, Ndx) ->
% 	NC = (length(Rs) > 1) andalso needs_cache(St),
% 	StW = maybe_wrap(St, NC),

% 	{StW1, A} = lists:foldl(fun(R, {St1, Acc}) ->
% 		RName = ouc_rstat_rule:get_name(R),
% 		{B, Intvls} = ouc_rstat_rule:get_covered_intervals(R, I),
% 		{St2, V} = get_props_for_intervals(St1, Ps, Intvls, Ndx),
% 		Acc1 = [{RName, B, V}|Acc],
% 		{St2, Acc1}
% 	end, {StW, []}, Rs),

% 	{maybe_unwrap(StW1, NC), A}.


% get_props_for_intervals(St, Ps, Intvls, {any, Ndxs}) ->
% 	AllPs = basic_prop_keys(),

% 	%% TODO add dpenedency
% 	Ps1 = [P || P <- AllPs, (not is_list(Ps)) orelse lists:member(P, Ps)],

% 	{St3, Vs} = lists:foldl(fun(P, {St1, Acc}) ->
% 		J = get_prop_join_fun(P),
% 		Keys = [{Intvl, P, Ndx} || Intvl <- Intvls, Ndx <- Ndxs],
% 		{St2, V} = fold_update_keys(Keys, J, 0, St1),
% 		Acc1 = [{P, V}|Acc],
% 		{St2, Acc1}
% 	end, {St, []}, Ps1),

% 	case Ps of
% 		all ->
% 			{St3, extend_props(Vs)};
% 		_ ->
% 			{St3, Vs}
% 	end;
% get_props_for_intervals(St, Ps, Intvls, Ndx) ->
% 	get_props_for_intervals(St, Ps, Intvls, {any, [Ndx]}).

% extend_props(Vs) ->
% 	ExtProps = extended_prop_keys(),

% 	Vs ++ [{P, derive(P, Vs)} || P <- ExtProps].

% derive({average, TotalK, CountK}, Vs) ->
% 	Total = proplists:get_value(TotalK, Vs, 0),
% 	Count = proplists:get_value(CountK, Vs, 0),
% 	safe_div(Total, Count);
% derive({percentage, TotalK, CountK}, Vs) ->
% 	Total = proplists:get_value(TotalK, Vs, 0),
% 	Count = proplists:get_value(CountK, Vs, 0),
% 	safe_div(Total*100, Count);

% %% custom
% derive({derived, occupancy}, Vs) ->
% 	[Idle, Ringing, OnCall, WrapUp] =
% 		[proplists:get_value(K, Vs, 0) || K <-
% 			[?PROP_TS_IDLE_DURATION,
% 			?PROP_TS_RINGING_DURATION,
% 			?PROP_TS_ONCALL_DURATION,
% 			?PROP_TS_WRAPUP_DURATION]],

% 	Busy = OnCall,
% 	Total = Idle + Ringing + OnCall + WrapUp,
% 	safe_div(Busy*100, Total).


% safe_div(_N, 0) -> infinity;
% safe_div(N, M) -> N/M.

% fold_update_keys([], _Jn, V, St) ->
% 	{St, V};
% fold_update_keys([Key|T], Jn, V, St) ->
% 	{St1, V1} = read(St, Key),
% 	Vt = Jn(V, V1),

% 	fold_update_keys(T, Jn, Vt, St1).

% maybe_wrap(St, false) -> St;
% maybe_wrap(St, _) ->
% 	{ok, StW} = ouc_rstat_store_dict:as_cache_for(St),
% 	StW.

% maybe_unwrap(St, false) -> St;
% maybe_unwrap({_, Dt}, _) ->
% 	ouc_rstat_store_dict:get_cache_for(Dt).

% %% access funs
% needs_cache({M, _}) ->
% 	M:needs_cache().

% read({M, Dt}, K) ->
% 	{Dt1, V} = M:read(Dt, K),
% 	{{M, Dt1}, V}.

% store_data(St, []) ->
% 	St;
% store_data({Store, StoreDt}, [{I, Ndx, {P, V}}|T]) ->
% 	Key = {I, P, Ndx},
% 	F = get_prop_join_action(P),
% 	StoreDt1 = Store:F(StoreDt, Key, V),
% 	store_data({Store, StoreDt1}, T).

% get_seconds_intersection({F1, T1}, {F2, T2}) ->
% 	min(T1, T2) - max(F1, F2) + 1.

% get_prop_for_acstate(released) -> ?PROP_TS_RELEASED_DURATION;
% get_prop_for_acstate(idle) -> ?PROP_TS_IDLE_DURATION;
% get_prop_for_acstate(ringing) -> ?PROP_TS_RINGING_DURATION;
% get_prop_for_acstate(oncall) -> ?PROP_TS_ONCALL_DURATION;
% get_prop_for_acstate(wrapup) -> ?PROP_TS_WRAPUP_DURATION.
