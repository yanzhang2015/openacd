-module(ouc_rstat_store_props).

-include("ouc_rstat.hrl").
-include_lib("proper/include/proper.hrl").

-compile([export_all]).

-define(M, ouc_rstat_conf).
-define(T, ouc_rstat_types).

-define(END_MIN, 10000).
-define(END_MAX, 10050).

-define(CLIENTS, ["bpi", "bdo"]).
-define(PROFILES, ["support", "admin"]).

%% types

maybe_client_ndx() -> oneof([[], [?LET(C, oneof(?CLIENTS), {client, C})]]).
maybe_profile_ndx() -> oneof([[], [?LET(P, oneof(?PROFILES), {profile, P})]]).


ndx() ->
	?LET({CNdx, PNdx}, {maybe_client_ndx(), maybe_profile_ndx()},
		lists:sort(CNdx ++ PNdx)).

ndx(C, P) ->
	?LET({CNdx, PNdx},
		{oneof([[], [{client, C}]]), oneof([[], [{profile, P}]])},
		lists:sort(CNdx ++ PNdx)).

ndxs() ->
	?LET({C, P}, {oneof(?CLIENTS), oneof(?PROFILES)},
		?LET(L, list(ndx(C, P)), lists:usort(L))
	).

start_instant() -> integer(?END_MIN, ?END_MAX).

call_log() ->
	?LET({End, AnsDur, ActiveDur, Ndxs},
		{start_instant(),
		integer(0, 100), integer(0, 500), ndxs()},
		begin
			%% Start happens when oncall begins, after answer
			Start = End - ActiveDur,
			{call, Start, End, AnsDur, Ndxs}
		end).

avail_log() ->
	?LET({End, Dur, Ndxs},
		{start_instant(), integer(0, 500), ndxs()},
		begin
			Start = End - Dur,
			{avail, Start, End, Ndxs}
		end).

%% props

prop_log_call_rstats_correct() ->
	?FORALL({Calls, Conf, INs}, {list(call_log()), ?T:conf(), list({start_instant(), ndx()})}, begin
		{ok, St} = ouc_rstat_store:new(ouc_rstat_store_dict, []),
		% mnesia:start(), {ok, St} = ouc_rstat_store:new(ouc_rstat_store_mnesia, []),

		St1 = lists:foldl(fun({_, Start, End, AnsDuration, Ndxs}, St2) ->
			ouc_rstat_store:log_call(St2, Conf, Start, End, AnsDuration, Ndxs)
		end, St, Calls),

		check_call_intervals(INs, St1, Conf, Calls)
	end).


prop_log_availability_rstats_correct() ->
	?FORALL({Avails, Conf, Is}, {list(avail_log()), ?T:conf(), list({start_instant(), ndx()})}, begin
		{ok, St} = ouc_rstat_store:new(ouc_rstat_store_dict, []),
		% mnesia:start(), {ok, St} = ouc_rstat_store:new(ouc_rstat_store_mnesia, []),

		St1 = lists:foldl(fun({_, Start, End, Ndxs}, St2) ->
			ouc_rstat_store:log_availability(St2, Conf, Start, End, Ndxs, End)
		end, St, Avails),

		check_availability_intervals(Is, St1, Conf, Avails)
	end).

check_availability_intervals(INs, St, Conf, Avails) ->
	Zs = [
		{?PROP_TS_AVAIL_DURATION, fun(F, T, N) ->
			AvailsIn = get_inclusive_avails(F, T, N, Avails),
			AvailDurs = [min(E-1, T) - max(S, F) + 1 || {S, E} <- AvailsIn],
			lists:sum(AvailDurs)
		end}
	],
	check_logs_tally(INs, St, Conf, Zs, Avails).

%% internal
check_call_intervals(INs, St, Conf, Calls) ->
	Zs = [
		{?PROP_TE_CALL_DURATION, fun(F, T, N) ->
			lists:sum(get_ended_call_durs(F, T, N, Calls))
		end},

		{?PROP_TE_CALL_COUNT, fun(F, T, N) ->
			length(get_ended_calls(F, T, N, Calls))
		end},

		{?PROP_TE_RING_DURATION, fun(F, T, N) ->
			CallsI = get_ended_calls(F, T, N, Calls),
			lists:sum([Ans || {_, _, _, Ans, _} <- CallsI])
		end},

		{?PROP_ME_CALL_DURATION, fun(F, T, N) ->
			case get_ended_call_durs(F, T, N, Calls) of
				[] -> 0;
				Durs -> lists:max(Durs)
			end
		end},

		{?PROP_TS_CALL_DURATION, fun(F, T, N) ->
			CallsIn = get_inclusive_calls(F, T, N, Calls),
			CallDurs = [min(E-1, T) - max(S, F) + 1 || {_, S, E, _, _} <- CallsIn],
			lists:sum(CallDurs)
		end}
	],
	check_logs_tally(INs, St, Conf, Zs, Calls).

check_logs_tally([], _, _, _, _) -> true;
check_logs_tally([{I, N}|INs], St, Conf, Zs, Logs) ->
	{St1, As} = ouc_rstat_store:get_props(St, Conf, I, N),

	Rs = ouc_rstat_conf:get_rules(Conf),

	Ok = lists:all(fun(R) ->
		RName = ouc_rstat_rule:get_name(R),

		{{F, T}=B, _} = ouc_rstat_rule:get_covered_intervals(R, I),

		case lists:keyfind(RName, 1, As) of
			{_, B, Ps} ->
				lists:all(fun({P, ExpF}) ->
					Ac = proplists:get_value(P, Ps),
					Exp = ExpF(F, T, N),

					check_eq({P, Exp}, {P, Ac})
				end, Zs);
			_ ->
				false
		end
	end, Rs),

	case Ok of
		true ->
			check_logs_tally(INs, St1, Conf, Zs, Logs);
		_ ->
			false
	end.

check_eq(Exp, Exp) -> true;
check_eq(Exp, Ac) ->
	io:format("ERROR: Expected: ~p, Actual: ~p~n", [Exp, Ac]),
	false.

get_ended_calls(F, T, N, Calls) ->
	[Call || Call={_, _, End, _, Ndxs} <- Calls, End >= F, End =< T, lists:member(N, Ndxs)].

get_ended_call_durs(F, T, N, Calls) ->
	get_call_durs(get_ended_calls(F, T, N, Calls)).

get_call_durs(Calls) ->
	[End - Start || {_, Start, End, _, _} <- Calls].

get_inclusive_calls(F, T, N, Calls) ->
	[Call || Call={_, S, E, _, Ndxs} <- Calls, (S =< T) and (E > F), lists:member(N, Ndxs)].

get_inclusive_avails(F, T, N, Avails) ->
	[{S, E} || {_, S, E, Ndxs} <- Avails, (S =< T) and (E > F), lists:member(N, Ndxs)].
