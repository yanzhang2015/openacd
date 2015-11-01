-module(ouc_db).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ouc_recordings.hrl").

-export([init/0, get_db/0, get_db/1, get_db_name/1, check_connection/0, exec_cmd/2]).

-define(DEF_REPLICA_SETS, ["127.0.0.1:27017"]).
-define(DB, ouc).
-define(N_CONN, 10).

-define(CONNECTION_CHECK_INTERVAL, 5000).


-spec(init() -> ok).
init() ->
	connect(),
	check_connection(),
	ensure_indices(),

	ok.

get_db() ->
	get_db(imdb).

get_db(Key) ->
	Name = get_db_name(Key),
	Pool = get_rand_pool_name(),
	mongoapi:new(Pool, Name).

get_db_name(imdb) ->
	<<"imdb">>;
get_db_name(openacd) ->
	%% Alias
	<<"reach">>;
get_db_name(reach) ->
	<<"reach">>;
get_db_name(reports) ->
	<<"reach_reports">>;
get_db_name(profiles) ->
	<<"profiles">>.

check_connection() ->
	IsConnectedF = fun(N) ->
		Pool = get_pool_name(N),
		mongodb:is_connected(Pool)
	end,

	case lists:any(IsConnectedF, lists:seq(1, ?N_CONN)) of
		true ->
			lager:info("DB ~p is connected", [?DB]),
			ok;
		false ->
			lager:info("DB ~p is not yet connected, retrying", [?DB]),
			connect(),
			timer:apply_after(?CONNECTION_CHECK_INTERVAL, ?MODULE,
				check_connection, [])
	end.

exec_cmd(Db, Cmd) ->
	Pool = get_rand_pool_name(),
	DbName = get_db_name(Db),
	mongodb:exec_cmd(Pool, DbName, Cmd).

%% Internal functions

connect() ->
	MongoCfg = os:getenv("MONGO_CONFIG_PATH"),
	ReplSets = case file:read_file(MongoCfg) of
		{ok, FileBin} ->
			FileStr = binary_to_list(FileBin),
			Lines = string:tokens(FileStr, "\n"),
			ConnectionUrl = lists:foldl(fun(L, Acc) ->
				case string:str(L, "connectionUrl=mongodb://") of
					1 ->
						L;
					_ ->
						Acc
				end
			end, [], Lines),
			case ConnectionUrl of
				[] ->
					?DEF_REPLICA_SETS;
				_ ->
					Start = length("connectionUrl=mongodb://") + 1,
					End = string:rstr(ConnectionUrl, "\/"),
					ReplHostsStr = string:substr(ConnectionUrl, Start, End - Start),
					string:tokens(ReplHostsStr, ",")
			end;
		_ ->
			?DEF_REPLICA_SETS
	end,
	lager:info("Trying to connect to ouc db replica sets: ~p", [ReplSets]),
	ConnF = fun(N) ->
		Pool = get_pool_name(N),
		mongodb:replicaSets(Pool, ReplSets),
		mongodb:connect(Pool)
	end,

	lists:foreach(ConnF, lists:seq(1, ?N_CONN)).


%% ---------------------------------------------------------------------------------
-spec(ensure_indices/0 :: () -> ok | error).
%% @doc Ensures that the collections where recordings will be saved
%%    exist in database, otherwise creates them.
%% @end
%% --------------------------------------------------------------------------------
ensure_indices() ->
	Db = get_db(reach),
	case catch Db:ensureIndex(?REC_META_COLL, [{<<"start_timestamp">>,1}]) of
		ok ->
			Db:gfsIndexes(),
			ok;
		Els1 ->
			lager:warning("Ensure indexing for recordings collection returned: ~p", [Els1]),
			error
	end,

	case catch Db:ensureIndex(<<?REC_GFS_BUCKET/binary, <<".">>/binary, ?REC_GFS_CHUNKS_COLL/binary>>,
		[{<<"files_id">>,1}, {<<"n">>, 1}], [{<<"unique">>, true}]) of
		ok ->
			ok;
		Els2 ->
			lager:warning("Ensure indexing for fd.chunks collection returned: ~p", [Els2]),
			error
	end.


get_rand_pool_name() ->
	N = crypto:rand_uniform(1, ?N_CONN + 1),
	get_pool_name(N).

get_pool_name(N) ->
	list_to_atom(atom_to_list(?DB) ++ integer_to_list(N)).

%% TESTS

-ifdef(TEST).

t_set_db(Db) ->
	application:set_env(reach_ouc, mongodb, Db).

t_connected() ->
	meck:called(mongodb, connect, [?DB], self()).

init_test_() ->
	{setup, fun() ->
		meck:new(mongodb),
		meck:expect(mongodb, replicaSets, 2, ok),
		meck:expect(mongodb, singleServer, 2, ok),
		meck:expect(mongodb, singleServer, 1, ok),
		meck:expect(mongodb, connect, 1, ok),
		meck:expect(mongodb, is_connected, 1, true)
	end, fun(_) ->
		meck:unload(mongodb)
	end, [{"replica_sets", fun() ->
		t_set_db({replica_sets, ["1.2.3.4:1234", "2.3.4.5:2345"]}),
		init(),
		?assert(meck:called(mongodb, replicaSets, [?DB, ["1.2.3.4:1234", "2.3.4.5:2345"]], self())),
		?assert(t_connected())
	end}, {"single with server", fun() ->
		t_set_db({single, "1.2.3.4:1234"}),
		init(),
		?assert(meck:called(mongodb, singleServer, [?DB, "1.2.3.4:1234"], self())),
		?assert(t_connected())
	end}, {"single without server", fun() ->
		t_set_db(single),
		init(),
		?assert(meck:called(mongodb, singleServer, [?DB], self())),
		?assert(t_connected())
	end}, {"default", fun() ->
		application:unset_env(reach_ouc, mongodb),
		init(),
		?assert(meck:called(mongodb, replicaSets, [?DB, ["127.0.0.1:27017"]], self())),
		?assert(t_connected())
	end}]}.

get_db_test_() ->
	[?_assertEqual(mongoapi:new(?DB, <<"imdb">>), get_db()),
	?_assertEqual(mongoapi:new(?DB, <<"imdb">>), get_db(imdb)),
	?_assertEqual(mongoapi:new(?DB, <<"profiles">>), get_db(profiles))].

-endif.
