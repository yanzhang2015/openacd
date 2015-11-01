-module(ouc_rstat_prop).

-export([
	basic_props/0,
	extended_props/0,

	is_basic/1,
	derive/2,
	join/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ouc_rstat.hrl").

basic_props() ->
	[
		?PROP_TE_CALL_COUNT,
		?PROP_TE_ABANDON_COUNT,
		?PROP_TE_ABANDON_DURATION,
		?PROP_TE_CALL_DURATION,
		?PROP_TE_RING_DURATION,
		?PROP_TE_WAIT_DURATION,
		?PROP_ME_CALL_DURATION,
		?PROP_ME_WAIT_DURATION,

		?PROP_TS_RELEASED_DURATION,
		?PROP_TS_IDLE_DURATION,
		?PROP_TS_RINGING_DURATION,
		?PROP_TS_ONCALL_DURATION,
		?PROP_TS_WRAPUP_DURATION
	].

extended_props() ->
	[
		?PROP_AvE_CALL_DURATION,
		?PROP_AvE_ABANDON_DURATION,
		?PROP_AvE_RING_DURATION,
		?PROP_AvE_WAIT_DURATION,
		?PROP_PcS_OCCUPANCY
	].

is_basic({Op, _, _}) when Op =:= total; Op =:= max ->
	true;
is_basic(_) ->
	false.

derive({average, XProp, YProp}, ReadF) ->
	YVal = ReadF(YProp),
	case YVal of
		0 ->
			infinity;
		Z when is_integer(Z)  ->
			XVal = ReadF(XProp),
			XVal / YVal;
		_ ->
			undefined
	end;
derive(?PROP_PcS_OCCUPANCY, ReadF) ->
	Released = ReadF(?PROP_TS_RELEASED_DURATION),
	Idle = ReadF(?PROP_TS_IDLE_DURATION),
	Ringing = ReadF(?PROP_TS_RINGING_DURATION),
	Wrapup = ReadF(?PROP_TS_WRAPUP_DURATION),

	OnCall = ReadF(?PROP_TS_ONCALL_DURATION),

	TotalEls = [Released, Idle, Ringing, Wrapup, OnCall],

	Busy = OnCall,

	case lists:all(fun erlang:is_integer/1, TotalEls) of
		true ->
			case lists:sum(TotalEls) of
				0 ->
					infinity;
				Total ->
					(100*Busy) / Total
			end;
		_ ->
			undefined
	end.
join({total, _, _}, []) ->
	0;
join({total, _, _}, Vs) ->
	try lists:sum(Vs)
	catch error:badarith -> undefined
	end;
join({max, _, _}, []) ->
	undefined;
join({max, _, _}, Vs) ->
	case lists:all(fun erlang:is_integer/1, Vs) of
		true ->
			lists:max(Vs);
		_ ->
			undefined
	end.

-ifdef(TEST).

-define(M, ?MODULE).

is_basic_test_() ->
	[
		?_assert(?M:is_basic(?PROP_TE_CALL_COUNT)),
		?_assert(?M:is_basic(?PROP_ME_CALL_DURATION)),
		?_assertNot(?M:is_basic(?PROP_AvE_CALL_DURATION))
	].

derive_average_test() ->
	ReadF = fun(?PROP_TE_CALL_DURATION) -> 100;
				(?PROP_TE_CALL_COUNT) -> 20 end,

	?assertEqual(5.0, ?M:derive(?PROP_AvE_CALL_DURATION, ReadF)).

derive_infinite_average_test() ->
	ReadF = fun(_) -> 0 end,
	?assertEqual(infinity, ?M:derive(?PROP_AvE_CALL_DURATION, ReadF)).

derive_undefined_average_test() ->
	ReadF = fun(_) -> undefined end,
	?assertEqual(undefined, ?M:derive(?PROP_AvE_CALL_DURATION, ReadF)).

derive_occupancy_test() ->
	ReadF = fun
		%% non-busy
		(?PROP_TS_RELEASED_DURATION) -> 120;
		(?PROP_TS_IDLE_DURATION) -> 30;
		(?PROP_TS_RINGING_DURATION) -> 10;
		(?PROP_TS_WRAPUP_DURATION) -> 40;

		(?PROP_TS_ONCALL_DURATION) -> 50
	end,
	?assertEqual(20.0, ?M:derive(?PROP_PcS_OCCUPANCY, ReadF)).

derive_empty_occupancy_test() ->
	ReadF = fun(_) -> 0 end,
	?assertEqual(infinity, ?M:derive(?PROP_PcS_OCCUPANCY, ReadF)).

derive_undefined_occupancy_test() ->
	ReadF = fun(_) -> undefined end,
	?assertEqual(undefined, ?M:derive(?PROP_PcS_OCCUPANCY, ReadF)).

join_test_() ->
	[?_assertEqual(50, ?M:join(?PROP_TE_CALL_COUNT, [30, 20])),
	?_assertEqual(12, ?M:join(?PROP_ME_CALL_DURATION, [12, 9])),

	% empty
	?_assertEqual(0, ?M:join(?PROP_TE_CALL_COUNT, [])),
	?_assertEqual(undefined, ?M:join(?PROP_ME_CALL_DURATION, [])),

	% invalid
	?_assertEqual(undefined, ?M:join(?PROP_TE_CALL_COUNT, [2, undefined])),
	?_assertEqual(undefined, ?M:join(?PROP_ME_CALL_DURATION, [2, undefined]))
	].

-endif.