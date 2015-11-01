-module(ouc_rstat_store_mnesia).
-behavior(ouc_rstat_store).

-include("ouc_rstat.hrl").

-define(TAB, ouc_rstat).

-export([
	get_store/0,

	init/1,
	increment/3,
	set_if_max/3,
	read/2,
	needs_cache/0,

	dirty_delete_ndx_match/1
]).

-record(ouc_rstat, {
	key :: rstat_key(),
	val :: number()
}).

get_store() ->
	{?MODULE, void}.

init(_) ->
	install(),
	mnesia:clear_table(?TAB),
	{ok, void}.

install() ->
	mnesia:create_table(?TAB, [
		{attributes, record_info(fields, ouc_rstat)},
		{type, set}
	]).

increment(_, K, N) ->
	mnesia:dirty_update_counter(?TAB, K, N),
	void.

set_if_max(_, K, N) ->
	mnesia:transaction(fun() ->
		M = case mnesia:read(ouc_rstat, K) of
			[#ouc_rstat{val=V}] -> max(V, N);
			_ -> N
		end,
		mnesia:write(#ouc_rstat{key=K, val=M})
	end),
	void.

read(_, K) ->
	V = case mnesia:dirty_read(ouc_rstat, K) of
		[#ouc_rstat{val=X}] -> X;
		_ -> 0
	end,
	{void, V}.

needs_cache() -> true.

dirty_delete_ndx_match(Ndx) ->
	lager:info("Deleting entries for ndx: ~p", [Ndx]),
	mnesia:activity(async_dirty, fun() ->
		Keys = [K || K={_, _, Ndx1} <- mnesia:all_keys(ouc_rstat), Ndx1 =:= Ndx],
		lists:foreach(fun(K) -> mnesia:delete(?TAB, K, write) end, Keys)
	end).