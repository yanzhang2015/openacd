-module(ouc_rstat_store_dict).

-behavior(ouc_rstat_store).

-export([
	init/1,
	increment/3,
	set_if_max/3,
	read/2,
	needs_cache/0,

	as_cache_for/1,
	get_cache_for/1
]).

%% for debug only
-export([
	dump/1
]).

-record(state_dt, {
	entries = dict:new() :: dict(),
	cache_for :: undefined | ouc_rstat_store:store()
}).

%% callbacks
init(Props) ->
	CacheFor = proplists:get_value(cache_for, Props),
	{ok, #state_dt{cache_for=CacheFor}}.

increment(St=#state_dt{entries=E}, K, N) ->
	E1 = dict:update(K, fun(V) -> V + N end, N, E),
	St#state_dt{entries=E1}.

set_if_max(St=#state_dt{entries=E}, K, N) ->
	E1 = dict:update(K, fun(V) -> erlang:max(V, N) end, N, E),
	St#state_dt{entries=E1}.

read(St=#state_dt{entries=E}, K) ->
	case dict:find(K, E) of
		{ok, V} ->
			{St, V};
		_ ->
			maybe_read_in_cache(St, K)
	end.

needs_cache() -> false.

%% api
as_cache_for(Store) ->
	ouc_rstat_store:new(?MODULE, [{cache_for, Store}]).

get_cache_for(#state_dt{cache_for=C}) ->
	C.

%% debug
dump({_, #state_dt{entries=E}}) ->
	dict:to_list(E).

%% internal
maybe_read_in_cache(St=#state_dt{cache_for=undefined}, _K) ->
	{St, 0};
maybe_read_in_cache(St=#state_dt{cache_for=C, entries=E}, K) ->
	{C1, V} = ouc_rstat_store:read(C, K),
	E1 = dict:store(K, V, E),
	St1 = St#state_dt{entries=E1, cache_for=C1},
	{St1, V}.
