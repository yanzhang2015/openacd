-module(ouc_rstat_conf_props).

-include("ouc_rstat.hrl").
-include_lib("proper/include/proper.hrl").

-compile([export_all]).

-define(M, ouc_rstat_conf).
-define(T, ouc_rstat_types).

prop_get_rules_has_all_rules() ->
	?FORALL(RRs, list(?T:raw_unchecked_rule()), begin
		{ok, C} = ?M:new([{rules, RRs}]),

		Xs = [ouc_rstat_rule:from_conf(X) || X <- RRs],

		Rs1 = [R || {ok, R} <- Xs],

		IsDup = fun(R) ->
			N = ouc_rstat_rule:get_name(R),
			SNs = lists:filter(fun(R1) ->
				ouc_rstat_rule:get_name(R1) =:= N
			end, Rs1),
			case SNs of
				[R|_] -> false;
				_ -> true
			end
		end,

		lists:usort([R || R <- Rs1, not IsDup(R)]) =:=
			lists:sort(?M:get_rules(C))
		%% TODO check len
	end).

%% TODO dup name check

prop_get_unique_update_intervals_only_contains_all_unique_intervals() ->
	?FORALL({C, I}, {?T:conf(), ?T:instant()}, begin
		Rs = ?M:get_rules(C),
		Us = ?M:get_unique_update_intervals(C, I, 0),

		Us1 = lists:flatten([ouc_rstat_rule:get_update_interval(R, I) || R <- Rs]),

		only_contains_all_unique(Us, Us1)
	end).

prop_get_unique_update_intervals_for_range_only_contains_all_unique_intervals() ->
	?FORALL({C, I}, {?T:conf(), ?T:interval()}, begin
		% io:format("CI: ~p:get_unique_update_intervals_for_range(~p, ~p)~n", [?M, C, I]),
		Rs = ?M:get_rules(C),
		Us = ?M:get_unique_update_intervals_for_range(C, I, 0),
		Us1 = lists:flatten([ouc_rstat_rule:get_update_intervals_for_range(R, I) || R <- Rs]),

		only_contains_all_unique(Us, Us1)
	end).

prop_get_covered_intervals_has_all_intervals() ->
	?FORALL({C, I}, {?T:conf(), ?T:instant()}, begin
		Rs = ?M:get_rules(C),
		CIs = ?M:get_covered_intervals(C, I),
		(length(Rs) =:= length(CIs)) and lists:all(fun(R) ->
			N = ouc_rstat_rule:get_name(R),
			Us = ouc_rstat_rule:get_covered_intervals(R, I),

			lists:member({N, Us}, CIs)
		end, Rs)
	end).

prop_get_dynamic_rule_names_pending_update_works() ->
	?FORALL({C, Last, D}, {?T:conf(), ?T:instant(), int()}, begin
		Next = Last + abs(D),

		Ps = ?M:get_dynamic_rule_names_pending_update(C, Last, Next),

		StRules = ?M:get_rule_names_by_type(C, static),
		DynRules = ?M:get_rule_names_by_type(C, dynamic),

		NoSt = lists:all(fun(N) -> not lists:member(N, Ps) end, StRules),
		OkDyn = lists:all(fun(N) ->
			{ok, R} = ?M:get_rule(C, N),

			V = lists:member(N, Ps),

			Z = ouc_rstat_rule:get_next_update(R, Last),
			E = (Last < Z) and (Z =< Next),
			V =:= E
		end, DynRules),
		NoSt and OkDyn
	end).

%% internal

only_contains_all_unique(Xs, Ys) ->
	lists:usort(Ys) =:= lists:sort(Xs).