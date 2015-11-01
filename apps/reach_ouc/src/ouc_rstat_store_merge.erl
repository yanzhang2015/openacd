-module(ouc_rstat_store_merge).

-behavior(ouc_rstat_store).

-include("ouc_rstat.hrl").

-define(TAB, ouc_rstat).

-export([
	init/1,
	increment/3,
	set_if_max/3,
	read/2,
	needs_cache/0
]).

-record(state, {
	stores = [] :: [atom()]
}).

init(Stores) ->
	{ok, #state{stores=Stores}}.

increment(_, _, _) ->
	throw(not_allowed).

set_if_max(_, _, _) ->
	throw(not_allowed).

needs_cache() -> false.

read(St = #state{stores=Stores}, K={_Intvl, P, _Ndx}) ->
	F = case ouc_rstat_store:get_prop_join_action(P) of
		set_if_max -> max;
		_ -> sum
	end,

	R = lists:F([begin
		{_, V} = ouc_rstat_store:read(Store, K),
		V
	end || Store <- Stores]),

	{St, R}.
