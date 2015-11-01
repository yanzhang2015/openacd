%% Copyright (c) 2012 eZuce, Inc. All rights reserved.

-module(spx_db).

-export([init/0, check_connection/0]).

-define(CONNECTION_CHECK_INTERVAL, 5000).
-define(DB, spx).
-define(DEF_REPLICA_SETS, ["127.0.0.1:27017"]).

init() ->
	connect(),
	check_connection(),

	ok.

check_connection() ->
	case catch mongodb:is_connected(?DB) of
		true ->
			lager:info("DB ~p is connected", [?DB]);
		_ ->
			lager:info("DB ~p is not yet connected, retrying", [?DB]),
			connect(),
			timer:apply_after(?CONNECTION_CHECK_INTERVAL, ?MODULE,
				check_connection, [])
	end.

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
	lager:info("Trying to connect to spx db replica sets: ~p", [ReplSets]),

	mongodb:replicaSets(spx, ReplSets),
	mongodb:connect(spx).
