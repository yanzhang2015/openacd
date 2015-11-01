-module(ouc_rstat_rule).

-compile({parse_transform, do}).
-include("ouc_rstat.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(rule, {
	name :: atom(),
	type = static :: static | dynamic,
	coverage :: pod(),
	update = live :: duration() | live,
	pivot = ?DEFAULT_PIVOT :: instant()
}).

-type rule() :: #rule{}.

-type conf_error() :: invalid_name | invalid_pivot | invalid_coverage.

-export([
	from_conf/1,

	get_name/1,
	get_coverage/1,
	get_update_frequency/1,
	get_pivot/1,
	get_type/1,

	get_covered_bounds/2,
	get_covered_intervals/2,
	get_refresh_intervals/3,

	get_update_interval/2,
	get_update_intervals_for_range/2,

	get_next_update/2
]).

%% TODO add limit for coverage, update and pivot so as not
%% to go before 0000-01-01.
%% Testing witch quickcheck with a high enough size exposes this problem

-spec from_conf(any()) -> {ok, rule()} | {error, conf_error()}.
from_conf({static, Name, RCoverage}) ->
	from_conf({static, Name, RCoverage, ?DEFAULT_PIVOT});
from_conf({static, Name, RCoverage, RPivot}) ->
	do([error_m ||
		check_name(Name),
		Coverage <- check_static_coverage(RCoverage),
		Pivot <- check_pivot(RPivot),
		return(#rule{
			name=Name,
			type=static,
			coverage=Coverage,
			update=Coverage,
			pivot=Pivot
		})
	]);
from_conf({dynamic, Name, RCoverage, RUpdateFreq}) ->
	from_conf({dynamic, Name, RCoverage, RUpdateFreq, ?DEFAULT_PIVOT});
from_conf({dynamic, Name, RCoverage, RUpdateFreq, RPivot}) ->
	do([error_m ||
		check_name(Name),
		Coverage <- check_dynamic_coverage(RCoverage),
		UpdateFreq <- check_update_freq(RUpdateFreq),
		case Coverage rem UpdateFreq of
			0 -> ok;
			_ -> {error, invalid_update_freq_for_coverage}
		end,
		Pivot <- check_pivot(RPivot),
		return(#rule{
			name=Name,
			type=dynamic,
			coverage=Coverage,
			update=UpdateFreq,
			pivot=Pivot
		})
	]).

%% Getters

-spec get_name(rule()) -> rule_name().
get_name(#rule{name=Name}) ->
	Name.

-spec get_coverage(rule()) -> pod().
get_coverage(#rule{coverage=Coverage}) ->
	Coverage.

-spec get_update_frequency(rule()) -> pod().
get_update_frequency(#rule{update=Update}) ->
	Update.

-spec get_pivot(rule()) -> instant().
get_pivot(#rule{pivot=P}) ->
	P.

-spec get_type(rule()) -> dynamic | static.
get_type(#rule{type=T}) ->
	T.

-spec get_covered_bounds(rule(), instant()) -> interval().
get_covered_bounds(#rule{type=static, coverage=C, pivot=P}, I) ->
	get_interval(C, P, I);
get_covered_bounds(R = #rule{update=U, coverage=C}, I) ->
	N = C div U, %% should be divisible

	{Lf, Lt} = get_update_interval(R, I-U),
	Sf = Lf - (U * (N-1)),

	{Sf, Lt}.


%% @todo -- rename this to get_covered_update_intervals
%% @doc given a rule R and instant I, gets {X, Y}
%% where X is the total coverage and Y is a list of all intervals
%% based on the update frequency that forms X
-spec get_covered_intervals(rule(), instant()) -> {interval(), [interval()]}.
get_covered_intervals(#rule{type=static, coverage=C, update=C, pivot=P}, I) ->
	%% coverage should be equal to update rule for all static
	%% so there's always ony 1 interval

	Intvl = get_interval(C, P, I),
	{Intvl, [Intvl]};
get_covered_intervals(#rule{update=U, coverage=C}=R, I) when is_integer(U) ->
	%% we expect U to be an integer all the time since
	%% all rules that pass by here should be dynamic

	N = C div U, %% should be divisible

	{Lf, Lt} = get_update_interval(R, I-U),
	Sf = Lf - (U * (N-1)),

	Bounds = {Sf, Lt} = get_covered_bounds(R, I),

	Intervals =
		[begin
			F = Sf + (U*X),
			T = F + U - 1,
			{F, T}
		end || X <- lists:seq(0, N-1)],
	{Bounds, Intervals}.

-spec get_refresh_intervals(rule(), Now::instant(), LastRefresh::instant()) ->
	{update, Bounds::interval(), Expired::[interval()], New::[interval()]} |
	{set, Bounds::interval(), [interval()]}.
get_refresh_intervals(_R, I, L) when L > I ->
	throw(badarg);
get_refresh_intervals(R = #rule{type=static}, I, _L) ->
	{Bounds, Intervals} = get_covered_intervals(R, I),
	{set, Bounds, Intervals};
get_refresh_intervals(#rule{update=U, coverage=C, pivot=P}, I, L) ->
	U1 = U - 1,

	%% computing for expired...
	{Lef, _} = get_interval(U, P, L-C),
	{Ief, _} = get_interval(U, P, I-C),
	Expired = [{F, F+U1} || F <- lists:seq(Lef, Ief-1, U)],

	%% computing for new...
	{Lnf, _} = get_interval(U, P, L),
	{Inf, _} = get_interval(U, P, I),
	New = [{F, F+U1} || F <- lists:seq(Lnf, Inf-1, U)],

	Bounds = {Ief, Inf-1},
	{update, Bounds, Expired, New}.

-spec get_update_interval(rule(), instant()) -> interval().
get_update_interval(#rule{update=U, pivot=P}, I) ->
	get_interval(U, P, I).

-spec get_update_intervals_for_range(rule(), interval()) -> [interval()].
get_update_intervals_for_range(R, {F, T}) when F =< T ->
	get_update_interval_while_lte(R, F, T, []).

%% @doc get the next update after the given instant
-spec get_next_update(rule(), interval()) -> now | instant().
get_next_update(#rule{type=static}, _I) -> now;
get_next_update(R, I) ->
	{_, T} = get_update_interval(R, I),
	T + 1.

%% internal

check_name(N) when is_atom(N) -> ok;
check_name(_) -> {error, invalid_name}.

check_static_coverage(R) ->
	case ouc_time:to_pod(R) of
		{ok, I} when is_integer(I), I > 0 -> {ok, I};
		{ok, {_, J}=E} when J > 0 -> {ok, E};
		_ -> {error, invalid_coverage}
	end.

check_dynamic_coverage(R) ->
	case to_positive_duration(R) of
		false -> {error, invalid_coverage};
		O -> O
	end.

check_update_freq(Ru) ->
	case to_positive_duration(Ru) of
		false -> {error, invalid_update_freq};
		O -> O
	end.

check_pivot(P) when is_integer(P) ->
	{ok, P};
check_pivot({LocalDtTm, local}) ->
	{ok, ouc_time:ldatetime_to_unixts(LocalDtTm)};
check_pivot({LocalDtTm, utc}) ->
	{ok, ouc_time:datetime_to_unixts(LocalDtTm)};
check_pivot(LocalDtTm) ->
	try ouc_time:datetime_to_unixts(LocalDtTm) of
		DtTm -> {ok, DtTm}
	catch _:_ ->
		{error, invalid_pivot}
	end.

to_positive_duration(J) ->
	case ouc_time:to_duration(J) of
		{ok, K}=E when K > 0 -> E;
		_ -> false
	end.

get_interval({day, N}, P, I) ->
	{DtP, TmP} = ouc_time:unixts_to_ldatetime(P),
	{DtI, TmI} = ouc_time:unixts_to_ldatetime(I),

	DaysP = calendar:date_to_gregorian_days(DtP),
	DaysI = calendar:date_to_gregorian_days(DtI),

	%% treat get_interval on days as if it were seconds
	{DaysX, DaysY1} = get_interval(N, DaysP, DaysI),
	DtX = calendar:gregorian_days_to_date(DaysX),

	IsDtIGtOrEqDtX = (DaysI > DaysX) orelse
		((DaysI =:= DaysX) andalso (calendar:time_to_seconds(TmI) >=
			calendar:time_to_seconds(TmP))),


	{DtFrom, DtTo} = case IsDtIGtOrEqDtX of
		true ->
			{DtX, calendar:gregorian_days_to_date(DaysY1 + 1)};
		_ ->
			{calendar:gregorian_days_to_date(DaysX-N), DtX}
	end,

	From = ouc_time:ldatetime_to_unixts({DtFrom, TmP}),
	To = ouc_time:ldatetime_to_unixts({DtTo, TmP}) - 1,
	{From, To};

get_interval({month, N}, P, I) ->
	{{Ys, Ms, _Ds} = DtS, Ts} = ouc_time:unixts_to_ldatetime(P),
	{{Yg, Mg, _Dg}, _Tg} = DtTmI = ouc_time:unixts_to_ldatetime(I),

	MonthsS = (Ys * 12) + (Ms - 1),
	MonthsI = (Yg * 12) + (Mg - 1),

	MonthsDiff = MonthsI - MonthsS,
	PeriodsDiff = MonthsDiff div N,

	TmpDt = ouc_time:shift_date(DtS, PeriodsDiff * N, month),
	{FromDt, ToDt} = case DtTmI >= {TmpDt, Ts} of
		true ->
			{TmpDt, ouc_time:shift_date(DtS, (PeriodsDiff+1) * N, month)};
		_ ->
			{ouc_time:shift_date(DtS, (PeriodsDiff-1) * N, month), TmpDt}
	end,

	From = ouc_time:ldatetime_to_unixts({FromDt, Ts}),
	To = ouc_time:ldatetime_to_unixts({ToDt, Ts}) - 1,
	{From, To};
get_interval(Dur, P, I) when is_integer(Dur), I >= P ->
	Diff = (I - P) rem Dur,
	From = I - Diff,
	End = From + Dur - 1,
	{From, End};
get_interval(Dur, P, I) when is_integer(Dur) ->
	Diff = ((P - I - 1) rem Dur) + 1,
	End = I + Diff - 1,
	From = End - Dur + 1,
	{From, End}.

get_update_interval_while_lte(R, F, T, Acc) ->
	I={S, E} = get_update_interval(R, F),
	Acc1 = [I|Acc],
	case (S =< T) and (T =< E) of
		true ->
			lists:reverse(Acc1);
		_ ->
			get_update_interval_while_lte(R, E+1, T, Acc1)
	end.

%% Tests

-ifdef(TEST).

-define(M, ?MODULE).

pivot_test_() ->
	PivEq = fun(Given, Res) ->
		{ok, R} = from_conf({static, today, {day, 1}, Given}),
		?_assertEqual(Res, get_pivot(R))
	end,

	[
		{"int", PivEq(10, 10)},
		{"plain datetime", PivEq({{2012, 2, 1}, {2, 3, 45}},
			ouc_time:datetime_to_unixts({{2012, 2, 1}, {2, 3, 45}}))},
		{"utc datetime", PivEq({{{2012, 2, 1}, {2, 3, 45}}, utc},
			ouc_time:datetime_to_unixts({{2012, 2, 1}, {2, 3, 45}}))},
		{"local datetime", PivEq({{{2012, 2, 1}, {2, 3, 45}}, local},
			ouc_time:ldatetime_to_unixts({{2012, 2, 1}, {2, 3, 45}}))}

	].

%% The following tests only apply when the current TZ is
%% follows the EST/EDT timezone.
%% In particular, the tests are only ran when the TZ set is
%% America/New_York.
%%
%% EST is UTC -5h
%% EDT is UTC -4h
%%
%% Note that for America/New_York, the DST changes for 2013
%% occur on the following:
%% * EST to EDT: Mar 10, 02:00 EST -> 03:00 EDT
%% * EDT to EST: Nov 3, 02:00 EST -> 01:00 EDT
%%
timezone_test_() ->
	case os:getenv("TZ") of
		"America/New_York" ->
			shift_day_test_1() ++ shift_month_test_1();
		_ ->
			?debugMsg("WARN: TZ test disabled. Set env TZ=America/New_York"),
			[]
	end.

 %% ran only when TZ=America/New_York
shift_day_test_1() ->
	Pv1 = {{{2013, 1, 1}, {1, 5, 0}}, local}, %% EST (-5H) / {{2013,1,1},{6,5,0}} -- UTC
	Pv2 = {{{2013, 1, 1}, {2, 10, 0}}, local}, %% EST (-5H) / {{2013,1,1},{7,10,0}} -- UTC

	[{Nm, fun() -> check_bounds({day, NDays}, Pv, UtcDt, ExpUtcFrom, ExpUtcTo) end}
		|| {Nm, NDays, Pv, UtcDt, ExpUtcFrom, ExpUtcTo} <- [
		{"EST", 1, Pv1,
			{{2013, 2, 5}, {8, 3, 0}}, %% UtcDt
			{{2013, 2, 5}, {6, 5, 0}}, %% ExpUtcFrom
			{{2013, 2, 6}, {6, 4, 59}}} %% ExpUtcTo
		, {"EDT", 1, Pv1,
			{{2013, 3, 27}, {8, 3, 0}},
			{{2013, 3, 27}, {5, 5, 0}},
			{{2013, 3, 28}, {5, 4, 59}}}
		, {"on previous interval", 1, Pv1,
			{{2013, 3, 27}, {5, 3, 0}},
			{{2013, 3, 26}, {5, 5, 0}},
			{{2013, 3, 27}, {5, 4, 59}}}
		, {"on boundary start", 1, Pv1,
			{{2013, 3, 26}, {5, 5, 0}},
			{{2013, 3, 26}, {5, 5, 0}},
			{{2013, 3, 27}, {5, 4, 59}}}
		, {"on boundary end", 1, Pv1,
			{{2013, 3, 27}, {5, 4, 59}},
			{{2013, 3, 26}, {5, 5, 0}},
			{{2013, 3, 27}, {5, 4, 59}}}
		, {"3 days, after pivot", 3, Pv1,
			{{2013, 1, 8}, {8, 3, 0}},
			{{2013, 1, 7}, {6, 5, 0}},
			{{2013, 1, 10}, {6, 4, 59}}}
		, {"3 days, before pivot", 3, Pv1,
			{{2012, 12, 25}, {8, 3, 0}}, %% previous yr
			{{2012, 12, 23}, {6, 5, 0}},
			{{2012, 12, 26}, {6, 4, 59}}}
		, {"3 days crossover EST to EDT (with 11 hr day)", 3, Pv1,
			{{2013, 3, 10}, {8, 3, 0}},
			{{2013, 3, 8}, {6, 5, 0}},
			{{2013, 3, 11}, {5, 4, 59}}}
		, {"3 days crossover EDT to EST (with 13 hr day)", 3, Pv1,
			{{2013, 11, 2}, {7, 11, 0}},
			{{2013, 10, 31}, {5, 5, 0}},
			{{2013, 11, 3}, {6, 4, 59}}}

		, {"EST to EDT boundary on lost hour - first day", 1, Pv2,
			{{2013, 3, 9}, {8, 12, 0}},
			{{2013, 3, 9}, {7, 10, 0}},
			{{2013, 3, 10}, {6, 59, 59}}} %% 01:59:59 EST
		, {"EST to EDT boundary on lost hour - next day", 1, Pv2,
			{{2013, 3, 10}, {8, 12, 0}},
			{{2013, 3, 10}, {7, 0, 0}}, %% 03:00:00 EDT
			{{2013, 3, 11}, {6, 9, 59}}}

		, {"EDT to EST boundary on double hour - first day", 1, Pv1,
			{{2013, 11, 2}, {7, 11, 0}},
			{{2013, 11, 2}, {5, 5, 0}},
			{{2013, 11, 3}, {6, 4, 59}}} %% 01:04:59 EDT
		, {"EDT to EST boundary on double hour - next day", 1, Pv1,
			{{2013, 11, 3}, {7, 8, 9}},
			{{2013, 11, 3}, {6, 5, 0}},  %% 01:05:00 EST
			{{2013, 11, 4}, {6, 4, 59}}}

	]].

%% ran only when TZ=America/New_York
shift_month_test_1() ->
	Pv1 = {{{2013, 1, 1}, {1, 5, 0}}, local}, %% {{2013,1,1},{6,5,0}} -- UTC

	%% for EST to EDT check
	%% shift occurs on 2013-03-10 02:00 EST / 03:00 EDT / 07:00 UTC
	Pv2 = {{{2013, 1, 10}, {2, 10, 0}}, local}, %% EST (-5H) / {{2013,1,1},{7,10,0}} -- UTC

	%% for EDT to EST check
	%% shift occurs on 2013-11-10 02:00 EDT / 01:00 EST / 06:00 UTC
	Pv3 = {{{2013, 1, 3}, {1, 10, 0}}, local}, %% EST (-5H) / {{2013,1,3},{6,10,0}} -- UTC

	%% end of month pivot test
	Pv4 = {{{2013, 1, 31}, {2, 10, 0}}, local}, %% EST (-5H) / {{2013,1,31},{7,10,0}} -- UTC

	[{Nm, fun() -> check_bounds({month, NMonths}, Pv, UtcDt, ExpUtcFrom, ExpUtcTo) end}
		|| {Nm, NMonths, Pv, UtcDt, ExpUtcFrom, ExpUtcTo} <- [
		{"EST", 1, Pv1,
			{{2013, 2, 8}, {8, 3, 0}}, %% UtcDt
			{{2013, 2, 1}, {6, 5, 0}}, %% ExpUtcFrom
			{{2013, 3, 1}, {6, 4, 59}}} %% ExpUtcTo
		, {"EDT", 1, Pv1,
			{{2013, 6, 27}, {8, 3, 0}},
			{{2013, 6, 1}, {5, 5, 0}},
			{{2013, 7, 1}, {5, 4, 59}}}
		, {"on previous interval", 1, Pv1,
			{{2013, 6, 1}, {5, 4, 0}},
			{{2013, 5, 1}, {5, 5, 0}},
			{{2013, 6, 1}, {5, 4, 59}}}
		, {"on boundary start", 1, Pv1,
			{{2013, 6, 27}, {5, 5, 0}},
			{{2013, 6, 1}, {5, 5, 0}},
			{{2013, 7, 1}, {5, 4, 59}}}
		, {"on boundary end", 1, Pv1,
			{{2013, 7, 1}, {5, 4, 59}},
			{{2013, 6, 1}, {5, 5, 0}},
			{{2013, 7, 1}, {5, 4, 59}}}
		, {"3 months, after pivot", 3, Pv1,
			{{2013, 8, 8}, {5, 3, 0}},
			{{2013, 7, 1}, {5, 5, 0}},
			{{2013, 10, 1}, {5, 4, 59}}}
		, {"3 months, before pivot", 3, Pv1,
			{{2012, 8, 8}, {5, 3, 0}}, %% previous year
			{{2012, 7, 1}, {5, 5, 0}},
			{{2012, 10, 1}, {5, 4, 59}}}
		, {"3 months, crossover EST to EDT (with 11 hr day)", 3, Pv1,
			{{2013, 1, 25}, {8, 3, 0}},
			{{2013, 1, 1}, {6, 5, 0}},
			{{2013, 4, 1}, {5, 4, 59}}}
		, {"3 months crossover EDT to EST (with 13 hr day)", 3, Pv1,
			{{2013, 11, 2}, {7, 11, 0}},
			{{2013, 10, 1}, {5, 5, 0}},
			{{2014, 1, 1}, {6, 4, 59}}}

		, {"EST to EDT boundary on lost hour - first month", 1, Pv2,
			{{2013, 2, 20}, {8, 12, 0}},
			{{2013, 2, 10}, {7, 10, 0}},
			{{2013, 3, 10}, {6, 59, 59}}} %% 01:59:59 EST
		, {"EST to EDT boundary on lost hour - next month", 1, Pv2,
			{{2013, 3, 20}, {8, 12, 0}},
			{{2013, 3, 10}, {7, 0, 0}}, %% 03:00:00 EDT
			{{2013, 4, 10}, {6, 9, 59}}}

		, {"EDT to EST boundary on double hour - first month", 1, Pv3,
			{{2013, 11, 1}, {7, 11, 0}},
			{{2013, 10, 3}, {5, 10, 0}},
			{{2013, 11, 3}, {6, 9, 59}}} %% 01:09:59 EDT
		, {"EDT to EST boundary on double hour - next month", 1, Pv3,
			{{2013, 12, 1}, {7, 8, 9}},
			{{2013, 11, 3}, {6, 10, 0}},  %% 01:10:00 EST
			{{2013, 12, 3}, {6, 9, 59}}}

		, {"end of month pivot - end of jan to end of feb", 1, Pv4,
			{{2013, 2, 5}, {7, 8, 9}},
			{{2013, 1, 31}, {7, 10, 0}},
			{{2013, 2, 28}, {7, 9, 59}}}
		, {"end of month pivot - end of feb to end of march", 1, Pv4,
			{{2013, 3, 5}, {7, 8, 9}},
			{{2013, 2, 28}, {7, 10, 0}},
			{{2013, 3, 31}, {6, 9, 59}}} %% EDT
	]].

check_bounds(Cov, Pivot, UtcDt, ExpUtcFrom, ExpUtcTo) ->
	{ok, R} = from_conf({static, rule1, Cov, Pivot}),
	{From, To} = get_covered_bounds(R, ouc_time:datetime_to_unixts(UtcDt)),

	UtcFrom = ouc_time:unixts_to_datetime(From),
	UtcTo = ouc_time:unixts_to_datetime(To),

	?assertEqual(ExpUtcFrom, UtcFrom),
	?assertEqual(ExpUtcTo, UtcTo).

get_covered_bounds_test_() ->
	I = ouc_time:datetime_to_unixts({{2013, 5, 20}, {03, 32, 10}}),
	{ok, StaticR} = from_conf({static, rule1, {minute, 30}, 0}),
	{ok, DynamicR} = from_conf({dynamic, rule2, {minute, 30}, 5, 0}),

	[{"static", fun() ->
		{F, T} = get_covered_bounds(StaticR, I),
		?assertEqual({{2013, 5, 20}, {03, 30, 00}}, ouc_time:unixts_to_datetime(F)),
		?assertEqual({{2013, 5, 20}, {03, 59, 59}}, ouc_time:unixts_to_datetime(T))
	end},
	{"dynamic", fun() ->
		{F, T} = get_covered_bounds(DynamicR, I),
		?assertEqual({{2013, 5, 20}, {03, 02, 10}}, ouc_time:unixts_to_datetime(F)),
		?assertEqual({{2013, 5, 20}, {03, 32, 09}}, ouc_time:unixts_to_datetime(T))
	end}].

get_refresh_intervals_test_() ->
	I = ouc_time:datetime_to_unixts({{2013, 5, 20}, {03, 32, 12}}),
	{ok, StaticR} = from_conf({static, rule1, {minute, 30}, 0}),
	{ok, DynamicR} = from_conf({dynamic, rule2, {minute, 30}, 5, 0}),

	[{"static - always set", fun() ->
		LastR = ouc_time:datetime_to_unixts({{2013, 5, 20}, {03, 30, 05}}),

		{set, {F, T}, [{F, T}]} = ?M:get_refresh_intervals(StaticR, I, LastR),
		?assertEqual({{2013, 5, 20}, {03, 30, 00}}, ouc_time:unixts_to_datetime(F)),
		?assertEqual({{2013, 5, 20}, {03, 59, 59}}, ouc_time:unixts_to_datetime(T))
	end}, {"dynamic - no updates", fun() ->
		LastR = tm2ts(03, 32, 10),

		{update, B, E, N} = ?M:get_refresh_intervals(DynamicR, I, LastR),

		?assertEqual({{03, 02, 10}, {03, 32, 09}}, intvl2tm(B)),

		?assertEqual([], E),
		?assertEqual([], N)
	end}, {"dynamic - with updates", fun() ->
		%% I: 03:32:12, B <- 03:02:10 to 03:32:09

		%% LastR: 03:32:02, LastB <- 03:02:00 to 03:31:59
		LastR = tm2ts(03, 32, 2),

		%% Expired: 03:02:00-04, 03:02:05-09
		%% New: 03:32:00-04, 03:32:05-09

		{update, B, [E1, E2], [N1, N2]} =
			?M:get_refresh_intervals(DynamicR, I, LastR),

		%% Bounds
		?assertEqual({{03, 02, 10}, {03, 32, 09}}, intvl2tm(B)),

		%% Expired
		?assertEqual({{03, 02, 00}, {03, 02, 04}}, intvl2tm(E1)),
		?assertEqual({{03, 02, 05}, {03, 02, 09}}, intvl2tm(E2)),

		%% New
		?assertEqual({{03, 32, 00}, {03, 32, 04}}, intvl2tm(N1)),
		?assertEqual({{03, 32, 05}, {03, 32, 09}}, intvl2tm(N2))
	end}, {"dynamic - badarg L < I", fun() ->
		LastR = tm2ts(03, 40, 2),
		?assertThrow(badarg, ?M:get_refresh_intervals(DynamicR, I, LastR))
	end}].

dttm2ts(DtTm) ->
	ouc_time:datetime_to_unixts(DtTm).

tm2ts(Hr, Min, Sec) ->
	dttm2ts({{2013, 05, 20}, {Hr, Min, Sec}}).

ts2dt(Ts) ->
	ouc_time:unixts_to_datetime(Ts).

ts2tm(Ts) ->
	{{2013, 05, 20}, Tm} = ts2dt(Ts),
	Tm.

intvl2tm({X, Y}) ->
	{ts2tm(X), ts2tm(Y)}.

%% property based tests
props_test_() ->
	ouc_test_util:props_test_(?MODULE).

-endif.
