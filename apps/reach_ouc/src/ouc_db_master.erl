%% ===========================================================================
%% @copyright 2014 eZuce Inc.
%% @author Costin-Tiberiu Radu <cradu@ezuce.com>
%% @doc Ouc db abstraction layer - exposes an unified view towards the db backends. <br />
%%      Current backends: redis; mongo 
%% @todo migrate ouc_db to ouc_db_mongo and make it a worker like ouc_db_redis 
%% @end
%% ===========================================================================
-module(ouc_db_master).


%% API
-export([
    get_connection/1,
    get_connection/2]).

%% ---------------------------------------------------------------------------
-spec get_connection(DbType) -> ConnectionHandler :: term()
    when 
        DbType :: redis | mongo.
%% @doc Returns a connection handler.
%% @todo Update the ouc_db mongo module name 
%% @end
%% ---------------------------------------------------------------------------
get_connection(redis) ->
    %% @todo what if the process is not started yet ??
    {Module, _WorkerPid} = ouc_db_redis:get_connection(),
    Module;

%% returns what mongoapi:new/2 returns
get_connection(mongo) ->
    ouc_db:get_db();

get_connection(DbType) ->
    %% lager:warning("Somebody tried to get a connection handler for an unsupported db type - ~p ", [DbType]).
    io:format("Somebody tried to get a connection handler for an unsupported db type - ~p ", [DbType]).

%% ----------------------------------------------------------------------------
-spec get_connection(atom(), atom()) -> ConnectionHandler::term().
%% @doc Returns a connection handler
%% @end
%% ----------------------------------------------------------------------------
get_connection(mongo, Database) ->
    ouc_db:get_db(Database);

get_connection(Other, _Db) ->
    lager:warning("A db of type ~p is not supported", [Other]).