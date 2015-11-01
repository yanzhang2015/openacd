-module(ouc_rstat_logger).

-include("ouc_rstat.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
	log_call/4,
	log_ustate/6
]).

log_call(Store, CallH, Ndxs, Ts) ->
	InitTime = get_tran_start(CallH, Ts),
	OncallInterval = get_oncall_interval(CallH),

	case OncallInterval of
		{OncallStart, OncallEnd} ->
			RingingTime = get_ring_start(CallH),

			WaitDuration = OncallStart - InitTime,
			AnsDuration = case RingingTime of
				undefined -> 0;
				_ -> max(OncallStart - RingingTime, 0)
			end,

			OncallDuration = OncallEnd - OncallStart,

			PVs = [
				{?PROP_TE_CALL_COUNT, 1},
				{?PROP_TE_CALL_DURATION, OncallDuration},
				{?PROP_TE_RING_DURATION, AnsDuration},
				{?PROP_TE_WAIT_DURATION, WaitDuration},
				{?PROP_ME_CALL_DURATION, OncallDuration},
				{?PROP_ME_WAIT_DURATION, WaitDuration}
			],

			ouc_rstat_store:multi_log(Store, OncallEnd, Ndxs, PVs);
		_ ->
			EndTime = get_tran_end(CallH, Ts),
			AbandonTime = max(EndTime - InitTime, 0),
			PVs = [
				{?PROP_TE_ABANDON_COUNT, 1},
				{?PROP_TE_ABANDON_DURATION, AbandonTime}
			],

			ouc_rstat_store:multi_log(Store, EndTime, Ndxs, PVs)
	end.

log_ustate(Store, _UState, S, S, _Ndxs, _Ts) ->
	%% If availability ended within the same second,
	%% that is, availability duration is < 1 sec,
	%% session is disregarded
	Store;
log_ustate(Store, UState, Start, End, Ndxs, _Ts) ->
	End1 = End - 1,
	Rg = {Start, End1},

	P = get_prop_for_ustate(UState),
	ouc_rstat_store:log_range(Store, Rg, Ndxs, P, fun(Intvl) ->
		get_seconds_intersection(Rg, Intvl)
	end).

%% internal

get_tran_start([], Def) ->
	Def;
get_tran_start([{_, Time}|_], _) ->
	Time.

%% gets the last ring start
%% WARNING: won't work right if transfers are present
get_ring_start(Trans) ->
	proplists:get_value(ringing, lists:reverse(Trans)).

get_oncall_interval([]) -> undefined;
get_oncall_interval([{oncall, Start}, {_, End}|_]) -> {Start, End};
get_oncall_interval([_|T]) -> get_oncall_interval(T).

get_tran_end([], Def) ->
	Def;
get_tran_end(Trans, _) ->
	{_, Time} = lists:last(Trans),
	Time.


get_seconds_intersection({F1, T1}, {F2, T2}) ->
	min(T1, T2) - max(F1, F2) + 1.

get_prop_for_ustate(released) -> ?PROP_TS_RELEASED_DURATION;
get_prop_for_ustate(idle) -> ?PROP_TS_IDLE_DURATION;
get_prop_for_ustate(ringing) -> ?PROP_TS_RINGING_DURATION;
get_prop_for_ustate(oncall) -> ?PROP_TS_ONCALL_DURATION;
get_prop_for_ustate(wrapup) -> ?PROP_TS_WRAPUP_DURATION.

-ifdef(TEST).

-define(M, ?MODULE).

%% tests

basic_log_call_test_() ->
	Now1 = dtm({05, 30, 01}),
	Now2 = dtm({05, 31, 16}),

	Store = t_log_calls([
		%% Single ring completed calls
		%%              (inivr/inqueue,ringing     , oncall      , wrapup      )
		t_completed_call({05, 00, 10}, {05, 00, 20}, {05, 01, 00}, {05, 01, 10}), % 01
		t_completed_call({05, 00, 12}, {05, 00, 18}, {05, 01, 20}, {05, 02, 10}), % 02
		t_completed_call({05, 00, 12}, {05, 00, 13}, {05, 01, 15}, {05, 02, 17}), % 03

		%% No ring abandoned call
		%%               inivr/inqueue, abandon time
		t_abandoned_call({05, 00, 11}, {05, 00, 18}), % 04
		t_abandoned_call({05, 00, 13}, {05, 00, 16})  % 05

	], Now1),

	R = fun(P, N) ->
		{ok, _, V} = ouc_rstat_store:read(Store, P, N, t_ndx(), last_30m),
		V
	end,
	R1 = fun(P) -> R(P, Now1) end,
	R2 = fun(P) -> R(P, Now2) end,

	%% wait_duration = oncall - inivr
	{"basic_log_call", [
		% %% Call#           01 02 03 04 05
		?_assertEqual(sum([01,01,01,00,00]), R1(?PROP_TE_CALL_COUNT)),
		?_assertEqual(sum([40,62,62,00,00]), R1(?PROP_TE_RING_DURATION)),
		?_assertEqual(sum([10,50,62,00,00]), R1(?PROP_TE_CALL_DURATION)),
		?_assertEqual(sum([50,68,63,00,00]), R1(?PROP_TE_WAIT_DURATION)),

		?_assertEqual(max([10,50,62,00,00]), R1(?PROP_ME_CALL_DURATION)),
		?_assertEqual(max([50,68,63,00,00]), R1(?PROP_ME_WAIT_DURATION)),

		%% Abandoned props
		?_assertEqual(sum([00,00,00,01,01]), R1(?PROP_TE_ABANDON_COUNT)),
		?_assertEqual(sum([00,00,00,07,03]), R1(?PROP_TE_ABANDON_DURATION)),

		%% Now2 (05:31:16) last_30m covers 05:01:15 to 05:31:14 (inclusive)
		?_assertEqual(sum([00,50,62,00,00]), R2(?PROP_TE_CALL_DURATION))
	]}.

log_ustate_test_() ->
	Now = dtm({05, 30, 01}),
	Now2 = dtm({05, 31, 12}),

	Store = t_log_ustate([
		{idle, {05, 01, 09}, {05, 01, 20}},
		{idle, {05, 01, 12}, {05, 01, 22}},

		{ringing, {05, 01, 09}, {05, 01, 11}},
		{oncall, {05, 01, 09}, {05, 01, 12}},
		{wrapup, {05, 01, 09}, {05, 01, 13}},

		{released, {05, 01, 09}, {05, 01, 14}}
	], Now),

	R = fun(P, N) ->
		{ok, _, V} = ouc_rstat_store:read(Store, P, N, t_ndx(), last_30m),
		V
	end,

	{"log ustate", [
		?_assertEqual(sum([11, 10]), R(?PROP_TS_IDLE_DURATION, Now)),

		%% Now2 (05:31:12) last_30m covers 05:01:10 to 05:31:09 (inclusive)
		?_assertEqual(sum([10, 10]), R(?PROP_TS_IDLE_DURATION, Now2)),

		%% other states
		?_assertEqual(2, R(?PROP_TS_RINGING_DURATION, Now)),
		?_assertEqual(3, R(?PROP_TS_ONCALL_DURATION, Now)),
		?_assertEqual(4, R(?PROP_TS_WRAPUP_DURATION, Now)),
		?_assertEqual(5, R(?PROP_TS_RELEASED_DURATION, Now))
	]}.

%% internal

dtm(Time) -> ouc_time:datetime_to_unixts({{2012, 5, 20}, Time}).

t_conf() ->
	{ok, Conf} = ouc_rstat_conf:new([{rules, [{dynamic, last_30m, {minute, 30}, 5}]}]),
	Conf.

t_ndx() ->
	[{agent, "agent0"}].

t_new_store() ->
	{ok, Store} = ouc_rstat_store:new(ouc_rstat_store_dict, t_conf(), []),
	Store.

t_completed_call(InIvrInQueue, Ringing, OnCall, WrapUp) ->
	[{inivr, dtm(InIvrInQueue)},
	{inqueue, dtm(InIvrInQueue)},
	{ringing, dtm(Ringing)},
	{oncall, dtm(OnCall)},
	{wrapup, dtm(WrapUp)},
	{endwrapup, dtm(WrapUp) + 5}].

t_abandoned_call(InIvrInQueue, HangupTime) ->
	[{inivr, dtm(InIvrInQueue)},
	{inqueue, dtm(InIvrInQueue)},
	{hangup, dtm(HangupTime)}].

t_log_calls(Calls, Now) ->
	t_log_calls(t_new_store(), Calls, Now).

t_log_calls(Store, CallHs, Now) ->
	lists:foldl(fun(CallH, StoreX) ->
		Ndxs = [[{agent, "agent0"}]],

		{ok, StoreY} = ?M:log_call(StoreX, CallH, Ndxs, Now),
		StoreY
	end, Store, CallHs).

t_log_ustate(Logs, Now) ->
	t_log_ustate(t_new_store(), Logs, Now).

t_log_ustate(Store, Logs, Now) ->
	Ndx = t_ndx(),
	Ndxs = [Ndx],

	lists:foldl(fun({CState, Start, End}, Acc) ->
		{ok, Acc1} = log_ustate(Acc, CState, dtm(Start), dtm(End), Ndxs, Now),
		Acc1
	end, Store, Logs).

sum(L) -> lists:sum(L).
max(L) -> lists:max(L).

-endif.

