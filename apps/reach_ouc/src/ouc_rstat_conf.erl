-module(ouc_rstat_conf).

-include("ouc_rstat.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
	new/1,
	get_rules/1,
	get_rule_names/1,
	get_rule/2,

	get_rule_names_by_type/2,

	get_unique_update_intervals/3,
	get_unique_update_intervals_for_range/3,
	get_covered_intervals/2,

	get_next_dynamic_update/2,
	get_dynamic_rule_names_pending_update/3
]).

-record(conf, {
	rules = [] :: [conf()]
}).

new(Props) ->
	RawRules = proplists:get_value(rules, Props, ?DEFAULT_CONF),
	Rules = read_rules(RawRules, []),

	{ok, #conf{
		rules = Rules
	}}.

get_rules(#conf{rules=Rs}) ->
	Rs.

get_rule_names(#conf{rules=Rs}) ->
	[ouc_rstat_rule:get_name(R) || R <- Rs].

get_rule(#conf{rules=Rs}, Name) ->
	case [R || R <- Rs, ouc_rstat_rule:get_name(R) =:= Name] of
		[R] -> {ok, R};
		_ -> none
	end.

get_rule_names_by_type(C, T) ->
	[ouc_rstat_rule:get_name(R) || R <- get_rules_by_type(C, T)].

get_rules_by_type(#conf{rules=Rs}, T) ->
	[R || R <- Rs, ouc_rstat_rule:get_type(R) =:= T].

%% @doc get intervals to be updated, excluding the ones that
%% will not be be covered from 'Now'
get_unique_update_intervals(#conf{rules=Rs}, I, Now) ->
	BIs = [{ouc_rstat_rule:get_covered_bounds(R, Now),
		ouc_rstat_rule:get_update_interval(R, I)} || R <- Rs],

	lists:usort([UIntvl || {{Cf, _Ct}, UIntvl = {_Uf, Ut}} <- BIs, Cf =< Ut]).

get_unique_update_intervals_for_range(#conf{rules=Rs}, Rg, Now) ->
	BIsz = [{ouc_rstat_rule:get_covered_bounds(R, Now),
		ouc_rstat_rule:get_update_intervals_for_range(R, Rg)} || R <- Rs],

	lists:usort(lists:append([ [UIntvl || UIntvl={_Uf, Ut} <- UIntvls, Cf =< Ut] ||
		{{Cf, _Ct}, UIntvls} <- BIsz])).

get_covered_intervals(#conf{rules=Rs}, I) ->
	[{ouc_rstat_rule:get_name(R), ouc_rstat_rule:get_covered_intervals(R, I)} || R <- Rs].

get_next_dynamic_update(C, I) ->
	Rs = get_rules_by_type(C, dynamic),
	case Rs of
		[] ->
			none;
		_ ->
			NUs = [{ouc_rstat_rule:get_name(R), ouc_rstat_rule:get_next_update(R, I)} || R <- Rs],
			Min = lists:min([U || {_, U} <- NUs]),
			NMins = [N || {N, U} <- NUs, U =:= Min],
			{Min, NMins}
	end.


-spec get_dynamic_rule_names_pending_update(conf(), LastUpdate::instant(), Now::instant()) -> [rule_name()].
get_dynamic_rule_names_pending_update(#conf{rules=Rs}, L, N) ->
	[ouc_rstat_rule:get_name(R) || R <- Rs, begin
		X = ouc_rstat_rule:get_next_update(R, L),
		is_integer(X) and (X =< N)
	end].

%% internal
read_rules([], Acc) ->
	lists:reverse(Acc);
read_rules([H|T], Acc) ->
	case ouc_rstat_rule:from_conf(H) of
		{ok, R} ->
			N = ouc_rstat_rule:get_name(R),
			Ns = [ouc_rstat_rule:get_name(A) || A <- Acc],
			case lists:member(N, Ns) of
				true ->
					lager:warning("Duplicate rolling stat name for rule: ~p", [H]),
					read_rules(T, Acc);
				_ ->
					read_rules(T, [R|Acc])
			end;
		{error, Err} ->
			lager:warning("Invalid rolling stat rule ~p: ~w", [H, Err]),
			read_rules(T, Acc)
	end.

%% Tests

-ifdef(TEST).

props_test_() ->
	ouc_test_util:props_test_(?MODULE).

-endif.
