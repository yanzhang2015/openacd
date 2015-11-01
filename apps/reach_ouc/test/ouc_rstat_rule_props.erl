-module(ouc_rstat_rule_props).

-compile([export_all]).

-include("ouc_rstat.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(M, ouc_rstat_rule).
-define(T, ouc_rstat_types).

%% get_update_interval

prop_get_update_interval_has_proper_length() ->
	?FORALL({I, R}, {?T:instant(), ?T:rule()}, begin
		U = ?M:get_update_frequency(R),
		P = ?M:get_pivot(R),

		{F, T} = ?M:get_update_interval(R, I),

		has_length(U, F, T, P)
	end).

prop_get_update_interval_is_recheable_from_pivot() ->
	?FORALL({I, R}, {?T:instant(), ?T:rule()}, begin
		U = ?M:get_update_frequency(R),
		P = ?M:get_pivot(R),

		{F, _} = ?M:get_update_interval(R, I),
		is_reachable(U, P, F)
	end).

prop_get_update_interval_contains_instant() ->
	?FORALL({I, R}, {?T:instant(), ?T:rule()},
		is_inclusive(?M:get_update_interval(R, I), I)).

%% get_covered_intervals
prop_get_covered_intervals_are_continuous() ->
	?FORALL({I, R}, {?T:instant(), ?T:rule()}, begin
		{_, Is} = ?M:get_covered_intervals(R, I),
		is_continuous(Is)
	end).

prop_get_covered_intervals_have_correct_bounds() ->
	?FORALL({I, R}, {?T:instant(), ?T:rule()}, begin
		{{F, T}, Is} = ?M:get_covered_intervals(R, I),

		[{S, _}|_] = Is,
		{_, E} = lists:last(Is),

		(F =:= S) and (T =:= E)
	end).

prop_get_covered_intervals_have_correct_lengths() ->
	?FORALL({I, R}, {?T:instant(), ?T:rule()}, begin
		U = ?M:get_update_frequency(R),
		P = ?M:get_pivot(R),

		{_, Intvs} = ?M:get_covered_intervals(R, I),

		lists:all(fun({F, T}) -> has_length(U, F, T, P) end, Intvs)
	end).

prop_get_covered_intervals_have_reachable_bounds() ->
	?FORALL({I, R}, {?T:instant(), ?T:rule()}, begin
		U = ?M:get_update_frequency(R),
		P = ?M:get_pivot(R),

		{{F, _}, _} = ?M:get_covered_intervals(R, I),
		is_reachable(U, P, F)
	end).

prop_get_covered_intervals_have_bounds_with_correct_length() ->
	?FORALL({I, R}, {?T:instant(), ?T:rule()}, begin
		C = ?M:get_coverage(R),
		P = ?M:get_pivot(R),

		{{F, T}, _} = ?M:get_covered_intervals(R, I),
		has_length(C, F, T, P)
	end).

prop_get_covered_intervals_for_static_rule_is_inclusive() ->
	?FORALL({I, R}, {?T:instant(), ?T:static_rule()}, begin
		{B, _} = ?M:get_covered_intervals(R, I),
		is_inclusive(B, I)
	end).

prop_get_covered_intervals_for_dynamic_rule_is_one_update_before() ->
	?FORALL({I, R}, {?T:instant(), ?T:dynamic_rule()}, begin
		{Bounds, _} = ?M:get_covered_intervals(R, I),
		UIntvl = ?M:get_update_interval(R, I),
		is_continuous([Bounds,UIntvl])
	end).

%% get_update_intervals_for_range

prop_get_update_intervals_for_range_is_continuous() ->
	?FORALL({T, R}, {?T:interval(), ?T:rule()}, begin
		is_continuous(?M:get_update_intervals_for_range(R, T))
	end).

prop_get_update_intervals_for_range_have_correct_lengths() ->
	?FORALL({G, R}, {?T:interval(), ?T:rule()}, begin
		U = ?M:get_update_frequency(R),
		P = ?M:get_pivot(R),

		Intvs = ?M:get_update_intervals_for_range(R, G),

		lists:all(fun({F, T}) -> has_length(U, F, T, P) end, Intvs)
	end).

prop_get_update_intervals_for_range_has_reachable_start() ->
	?FORALL({G, R}, {?T:interval(), ?T:rule()}, begin
		U = ?M:get_update_frequency(R),
		P = ?M:get_pivot(R),

		[{F, _}|_] = ?M:get_update_intervals_for_range(R, G),
		is_reachable(U, P, F)
	end).

prop_get_update_intervals_for_range_is_inclusive() ->
	?FORALL({{F, T}=G, R}, {?T:interval(), ?T:rule()}, begin
		Intvs = ?M:get_update_intervals_for_range(R, G),
		[IntvS|_] = Intvs,
		IntvL = lists:last(Intvs),
		is_inclusive(IntvS, F) and is_inclusive(IntvL, T)
	end).


%% internal

matches_pod(Pd, X) ->
	case ouc_time:to_pod(X) of
		{ok, Pd} -> true;
		_ -> false
	end.

is_reachable(U, From, I) when is_integer(U) ->
	(From - I) rem U =:= 0;
is_reachable({day, N}, From, I) ->
	FDtTm = {Fdt, Ftm} = ouc_time:unixts_to_ldatetime(From),
	T1DtTm = {T1dt, T1tm} = ouc_time:unixts_to_ldatetime(I),

	Diff = calendar:date_to_gregorian_days(T1dt) - calendar:date_to_gregorian_days(Fdt),
	((Diff rem N) =:= 0) and
		((Ftm =:= T1tm) or
			is_on_lost_hr(FDtTm) or
			is_on_lost_hr(T1DtTm));
is_reachable({month, M}, From, I) ->
	{{Fy, Fm, _}=Fdt, Ftm} = ouc_time:unixts_to_ldatetime(From),
	{{Iy, Im, _}=Idt, Itm} = ouc_time:unixts_to_ldatetime(I),

	FMonths = (Fy*12) + (Fm-1),
	IMonths = (Iy*12) + (Im-1),

	IsSameTime = Ftm =:= Itm,
	IsDaySimilar = is_day_similar(Fdt, Idt),

	IsMonthsReachable = ((IMonths - FMonths) rem M) =:= 0,

	IsSameTime and IsDaySimilar and IsMonthsReachable.

has_length(L, F, T, _P) when is_integer(L) ->
	(T - F + 1) =:= L;
has_length({day, N}, F, T, _P) ->
	FDtTm = {Fdt, Ftm} = ouc_time:unixts_to_ldatetime(F),
	T1DtTm = {T1dt, T1tm} = ouc_time:unixts_to_ldatetime(T + 1),

	Diff = calendar:date_to_gregorian_days(T1dt) - calendar:date_to_gregorian_days(Fdt),
	(Diff =:= N) and
		((Ftm =:= T1tm) or
			is_on_lost_hr(FDtTm) or
			is_on_lost_hr(T1DtTm));
has_length({month, M}, F, T, P) ->
	FDtTm = {{Fy, Fm, _}=Fdt, Ftm} = ouc_time:unixts_to_ldatetime(F),
	T1DtTm = {{T1y, T1m, _}=T1dt, T1tm} = ouc_time:unixts_to_ldatetime(T + 1),
	{Pdt, _} = ouc_time:unixts_to_ldatetime(P),

	FMonths = (Fy*12) + (Fm-1),
	T1Months = (T1y*12) + (T1m-1),

	(FMonths + M =:= T1Months) and
		((Ftm =:= T1tm) or
			is_on_lost_hr(FDtTm) or
			is_on_lost_hr(T1DtTm)) and is_day_similar(Pdt, Fdt) and is_day_similar(Pdt, T1dt).

is_on_lost_hr(DtTm) ->
	calendar:local_time_to_universal_time_dst(DtTm) =:= [].

%% not transitive!
%% @doc checks if dates X and Y are exactly N months apart, that is it follows any:
%% * day of X = the date of Y
%% * Y is on the last day of the month and the day of X is not available on the month of Y
is_day_similar({Xy, Xm, Xd}, {Yy, Ym, Yd}) ->
	(Xd =:= Yd) or
		begin
			IsXLastDayOfMonth = calendar:last_day_of_the_month(Xy, Xm) =:= Xd,
			IsYLastDayOfMonth = calendar:last_day_of_the_month(Yy, Ym) =:= Yd,

			IsYyYmXdInvalid = not calendar:valid_date(Yy, Ym, Xd),

			IsYLastDayOfMonth and (IsXLastDayOfMonth or IsYyYmXdInvalid)
		end.

is_continuous([]) -> true;
is_continuous([_]) -> true;
is_continuous(L) ->
	[{_, X}|L1] = L,
	[{Y, _}|_] = L1,

	case X+1 of
		Y -> is_continuous(L1);
		_ -> false
	end.

is_inclusive({F, T}, I) ->
	(F =< I) and (I =< T).

is_positive_duration(K) ->
	case ouc_time:to_duration(K) of
		{ok, D} when D > 0 -> true;
		_ -> false
	end.

is_positive_pod(K) ->
	case ouc_time:to_pod(K) of
		{ok, D} when is_integer(D), D > 0 -> true;
		{ok, {_, J}} when J > 0 -> true;
		_ -> false
	end.