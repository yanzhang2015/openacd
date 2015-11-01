-module(ouc_time).

-include("ouc_rstat.hrl").

-export([
	now/0,
	now/1,

	now_ms/0,
	now_ms/1,

	now_micro/0,
	now_micro/1,

	now_diff/2,

	datetime_to_unixts/1,
	unixts_to_datetime/1,

	ldatetime_to_unixts/1,
	unixts_to_ldatetime/1,

	erlnow_to_unixts/1,
	unixts_to_erlts/1,

	to_duration/1,
	to_period/1,
	to_pod/1,

	shift_date/3,

	to_sstring/1,
	to_erl_now/1
]).

-ifdef(TEST).
-export([
	set_now/1
]).
-endif.

-spec now() -> integer().
now() ->
	now(ts()).

-spec now(erlang:timestamp()) -> integer().
now({Mega, Sec, _}) ->
	Mega * 1000000 + Sec.

-ifndef(TEST).
ts() ->
	os:timestamp().
-else.
ts() ->
	case application:get_env(oacd_ouc, ouc_time_now) of
		{ok, V} -> to_erl_now(V);
		_ -> os:timestamp()
	end.
-endif.

-spec now_ms() -> integer().
now_ms() ->
	now_ms(os:timestamp()).

-spec now_ms(erlang:timestamp()) -> integer().
now_ms({Mega, Sec, Micro}) ->
	(Mega * 1000000000) + (Sec * 1000) + (Micro div 1000).

-spec now_micro() -> integer().
now_micro() ->
	now_micro(os:timestamp()).

-spec now_micro(erlang:timestamp()) -> integer().
now_micro({Mega, Sec, Micro}) ->
	(Mega * 1000000000000) + (Sec * 1000000) + Micro.

-spec now_diff(erlang:timestamp(), erlang:timestamp()) -> integer().
now_diff(T2, T1) ->
	timer:now_diff(T2, T1) div 1000000.

-spec datetime_to_unixts(datetime()) -> instant().
datetime_to_unixts(DtTm) ->
	calendar:datetime_to_gregorian_seconds(DtTm) -
		?UNIX_EPOCH_GSEC.


-spec unixts_to_datetime(instant()) -> datetime().
unixts_to_datetime(UnixTs) ->
	calendar:gregorian_seconds_to_datetime(UnixTs +
		?UNIX_EPOCH_GSEC).

-spec erlnow_to_unixts(erlang:timestamp()) -> integer().
erlnow_to_unixts({Mega, Sec, _}) ->
	Mega * 1000000 + Sec.

-spec unixts_to_erlts(integer()) -> erlang:timestamp().
unixts_to_erlts(UnixTs) ->
	{UnixTs div 1000000, UnixTs rem 1000000, 0}.

-ifdef(TEST).
%% @todo update and expand unittests
set_now(Now) ->
	application:set_env(oacd_ouc, ouc_time_now, Now).
-endif.

%% local times

%% @doc converts the local datetime to a unix timestamp (utc) with
%% the following assumptions for DST:
%%
%% * if the local time has two utc equivalents, the standard time is chosen
%% * if the local time falls between the lost hour when transitioning to dst
%%   the next valid time is chosen
-spec ldatetime_to_unixts(datetime()) -> instant().
ldatetime_to_unixts(LDtTm) ->
	case calendar:local_time_to_universal_time_dst(LDtTm) of
		[X] -> datetime_to_unixts(X);
		[_, X] -> datetime_to_unixts(X); %% use the latter (standard) time
		[] ->
			%% treat local time as if it were UTC for a moment...
			{LDt, {LHr, LMin, _}} = LDtTm,
			LDtTm0Sec = {LDt, {LHr, LMin, 0}},

			LDtTm2 = calendar:gregorian_seconds_to_datetime(
				calendar:datetime_to_gregorian_seconds(LDtTm0Sec) + 60),

			ldatetime_to_unixts(LDtTm2)
	end.

-spec unixts_to_ldatetime(instant()) -> datetime().
unixts_to_ldatetime(UnixTs) ->
	calendar:universal_time_to_local_time(unixts_to_datetime(UnixTs)).

to_pod(X) ->
	case to_duration(X) of
		false -> to_period(X);
		O -> O
	end.

-spec shift_date(datetime(), integer(), month | day) -> datetime().
shift_date({Y, M, D}, N, month) ->
	Months = (Y*12) + (M-1) + N,
	Y1 = Months div 12,
	M1 = (Months rem 12) + 1,
	find_valid_date(Y1, M1, D);

shift_date(Dt, N, day) ->
	calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Dt) + N).

%% short string -- useful only for debug msgs
to_sstring(UnixTs) ->
	{{Y, Mo, D}, {H, Mn, S}} = unixts_to_datetime(UnixTs),
	io_lib:format("~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b", [Y, Mo, D, H, Mn, S]).

to_erl_now(UnixTs) ->
	{UnixTs div 1000000, UnixTs rem 1000000, 0}.

find_valid_date(Y, M, D) when D > 0 ->
	case calendar:valid_date(Y, M, D) of
		true ->
			{Y, M, D};
		_ ->
			find_valid_date(Y, M, D-1)
	end;
find_valid_date(_, _, _) ->
	throw(out_of_bounds).

to_duration({hour, N}) ->
	{ok, N * 60 * 60};
to_duration({minute, N}) ->
	{ok, N * 60};
to_duration({second, N}) ->
	{ok, N};
to_duration(N) when is_integer(N) ->
	{ok, N};
to_duration(_) ->
	false.

to_period({day, _}=P) -> {ok, P};
to_period({month, _}=P) -> {ok, P};
to_period({year, Y}) -> {ok, {month, Y*12}};
to_period(_) -> false.
