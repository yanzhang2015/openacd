%%%-------------------------------------------------------------------
%%% @author ctradu
%%% @copyright (C) 2014, <eZuce Inc.>
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2014 17:13
%%%-------------------------------------------------------------------
-module(ouc_call_recording_migration).
-author("ctradu").

-include_lib("reach_ouc/include/ouc_recordings.hrl").

%% API
-export([start/0,
        init/0]).

%%--------------------------------------------------------------------
%% @doc Start this task
%% @end
%%--------------------------------------------------------------------
-spec(start() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start() ->
    spawn(?MODULE, init, []).

%%--------------------------------------------------------------------
%% @doc Executes the actual migration task
%% @end
%%--------------------------------------------------------------------
-spec(init() -> {ok, Result::term()}).
init() ->
    %% if the migration was allready done - terminate
    %% else do the migration
    %% Get MongoDB connection to the collection 'reach'
    DbReach = ouc_db_master:get_connection(mongo, reach),
    case check_migration(DbReach) of
        false -> do_migration(DbReach), ok;
        _true -> ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% -------------------------------------------------------------------
%% @private
%% @doc Does the migraton of recordings
%% @end
%% -------------------------------------------------------------------
-spec(do_migration(DbReach::term()) -> ok).
do_migration(DbReach) ->
    %% get all recordings metadata from reach.recordings
    %% for each recording upload it to gridfs
    %% log successful and unsucessful uploads
    {{Y, M, D},{Hour, Min, Sec}} = calendar:local_time(),
    lager:notice("Starting migration on ~2..0B-~2..0B-~4..0B at ~2..0B:~2..0B:~2..0B",[D, M, Y, Hour, Min, Sec]),
    RecMeta = get_recording_meta(DbReach),

    case RecMeta of
        [] ->
            lager:notice("No recordings to migrate"),
            metadata_migrated(DbReach),
            ok;
        AllRecs ->
            ClientsDirs = get_clients_dirs(),
            MigrateGenFun = fun(Db, Dirs) ->
                fun(Rec) ->
                    migrate_recording(Db, Dirs, Rec)
                end
            end,
            MigrateFun = MigrateGenFun(DbReach, ClientsDirs),
            Result = lists:map(MigrateFun, AllRecs),
            Successful = lists:sum(Result),
            Total      = length(Result),
            lager:notice("A total of ~p recordings were processed and ~p were uploaded successfuly",[Total, Successful]),
            metadata_migrated(DbReach)
    end,
    {{Y, M, D},{HourEnd, MinEnd, SecEnd}} = calendar:local_time(),
    lager:notice("Finished migration on  ~2..0B-~2..0B-~4..0B at ~2..0B:~2..0B:~2..0B",[D, M, Y, HourEnd, MinEnd, SecEnd]).

%% -------------------------------------------------------------------------------
%% @private
%% @doc Migrates the recording described in RecMeta from disk to GridFS
%% @end
%% -------------------------------------------------------------------------------
-spec(migrate_recording(DbReach::term(), ClientsDirs::tuple(), RecMeta::list()) -> {ok, fail}).
migrate_recording(DbReach, ClientDirs, RecMeta) ->
    %% if upload is successful -> ok
    %%          else -> fail
    %% get recording path
    RecordingPath = get_recording_path(ClientDirs, RecMeta),
    case file:read_file(RecordingPath) of
        {ok,  BinFile} ->
            GridFsPid = DbReach:gfsNew(?REC_GFS_BUCKET, RecordingPath, [{chunkSize, ?REC_CHUNK_SIZE}]),
            lager:info("Uploading old recording to GridFS from location ~p", [RecordingPath]),
            DbReach:gfsWrite(GridFsPid, BinFile),
            DbReach:gfsClose(GridFsPid),
            metadata_update(DbReach, RecMeta, RecordingPath, <<"true">>),
            1; %% true

        {error, Reason} ->
            lager:error("Error while trying to save the recording ~p in GridFS: ~p", [RecordingPath, Reason]),
            metadata_update(DbReach, RecMeta, RecordingPath, <<"false">>),
            0 %% false
    end.

%% -------------------------------------------------------------------------------
%% @doc Gets all recording metadata from Mongodb reach.recording
%% @end
%% -------------------------------------------------------------------------------
-spec(get_recording_meta(DbReach::term()) -> list()).
get_recording_meta(DbReach) ->
    {ok, AllRecs} = DbReach:find(<<"recordings">>, [], [], 0, 0),
    lager:info("Found ~p recordings metadata from a previous version in MongoDB",[length(AllRecs)]),
    AllRecs.


%% -------------------------------------------------------------------------------
%% @private
%% @doc Get disk path for a given recording from the metadata
%% @end
%% -------------------------------------------------------------------------------
-spec(get_recording_path(ClientsDirs::list(), RecMeta::list()) -> list()).
get_recording_path(ClientsDirs, RecMeta) ->
    RecLocationUri = proplists:get_value(<<"recording_location">>, RecMeta),
    RecFileType    = proplists:get_value(<<"recording_file_type">>, RecMeta, <<"wav">>),
    ClientId       = proplists:get_value(<<"client_id">>, RecMeta),
    StartTimestamp = proplists:get_value(<<"start_timestamp">>, RecMeta),
    ClientDirBin   = proplists:get_value(ClientId, ClientsDirs),
    ClientDir      = binary:bin_to_list(ClientDirBin),

    RecLocationUriS = binary:bin_to_list(RecLocationUri),
    SplitOne = string:tokens(RecLocationUriS, "?"),
    [_PrefixOne, SufixOne] = SplitOne,
    SplitTwo = string:tokens(SufixOne,"&"),
    UriFun = fun(Elem) ->
        KV = string:tokens(Elem, "="),
        [K, V] = KV,
        {K,V}
    end,
    UriProp = lists:map(UriFun, SplitTwo),
    UriCallID   = proplists:get_value("call_id", UriProp),

    {{Y,M,D}, {H,Mi,S}} = calendar:now_to_local_time(StartTimestamp),
    DateIo = io_lib:format("~B-~2..0B-~2..0B", [Y,M,D]),
    TimeIo = io_lib:format("~2..0B~2..0B~2..0B", [H,Mi,S]),
    StrDir = string:strip(ClientDir, right, $/),
    FullPath = lists:flatten(io_lib:format("~s/~s/~s/~s-~s.~s",
        [StrDir, ClientId, DateIo, TimeIo, UriCallID, RecFileType])),
    lager:debug("Recording path: ~p", [FullPath]),
    FullPath.


%% ----------------------------------------------------------------------------------------------
%% @private
%% @doc Queries MongoDB for all the clients configured in the system and returns a list of tuples.
%% ```
%%      [
%%       {<<"client-ident-one">>, <<"client-callsDir">>},
%%       {<<"client-one">>, <<"/var/sipxdata/reach/recordings">>}
%%      ]
%% '''
%% @end
%% ----------------------------------------------------------------------------------------------
-spec(get_clients_dirs() -> list()).
get_clients_dirs() ->
    DbImdb = ouc_db_master:get_connection(mongo, imdb),
    {ok, ClientsMeta} = DbImdb:find(<<"entity">>, [{<<"ent">>, <<"openacdclient">>}],[], 0, 0),
    ExtractFun = fun(Client) ->
        ClientIdent     = proplists:get_value(<<"ident">>, Client),
        ClientCallsDir  = proplists:get_value(<<"callsDir">>, Client),
        {ClientIdent, ClientCallsDir}
    end,
    ClientsDirs = lists:map(ExtractFun, ClientsMeta),
    ClientsDirs.

%% -------------------------------------------------------------------------------------
%% @private
%% @doc Verifies if the recordings were migrated once from disk to GridFS
%% @end
%% -------------------------------------------------------------------------------------
-spec(check_migration(DbReach::term()) -> boolean()).
check_migration(DbReach) ->
    %% Looking for collection rec_migrated
    {ok, MigratedLi} = DbReach:findOne(<<"rec_migrated">>, [{<<"migration_executed">>, true}]),
    % lager:debug("rec_migrated -> migration_executed: ~p",[MigratedLi]),
    Res = case MigratedLi of
        [] -> false;
        Migrated ->
            Ret = proplists:get_value(<<"migration_executed">>, Migrated),
            case Ret of
                true ->
                    lager:notice("Call recording migration allready done."),
                    true;
                _ -> false
            end
    end,
    Res.

%% -----------------------------------------------------------------------------------------
%% @private
%% @doc Updates metadata for one migrated recording
%% @end
%% -----------------------------------------------------------------------------------------
-spec(metadata_update(DbReach::tuple(), RecMeta::list(), RecordingPath::string(), binary()) -> boolean()).
metadata_update(DbReach, RecMeta, RecordingPath, Success)->
    %% if db_uploaded exists, update it
    %% else add it
    UploadedEntry = [{<<"db_uploaded">>, Success}],
    FilenameEntry = [{<<"filename">>,list_to_binary(RecordingPath)}],

    NewRecOne = case proplists:get_value(<<"db_uploaded">>,RecMeta) of
        undefined      -> RecMeta ++ UploadedEntry;
        _Other  -> lists:keyreplace(<<"db_uploaded">>, 1, RecMeta, {<<"db_uploaded">>, Success})
    end,

    NewRec = case proplists:get_value(<<"filename">>, RecMeta) of
        undefined      -> NewRecOne ++ FilenameEntry;
        _Any    -> lists:keyreplace(<<"filename">>, 1, NewRecOne, {<<"filename">>,  list_to_binary(RecordingPath)})
    end,
    RecId = proplists:get_value(<<"_id">>, RecMeta),
    % lager:debug("New Recording Meta: ~p",[NewRec]),

    DbReach:update(<<"recordings">>, [{<<"_id">>, RecId}], NewRec, [upsert]),
    true.

%% ---------------------------------------------------------------------------------------------------
%% @private
%% @doc Adds the rec_migrated -> migration_excuted to 'true' when the recording migration is finished
%% @end
%% ---------------------------------------------------------------------------------------------------
-spec(metadata_migrated(DbReach::tuple()) -> atom()).
metadata_migrated(DbReach) ->
    DbReach:update(<<"rec_migrated">>,
                  [{<<"migration_executed">>, {exists, false}}],
                  [{<<"migration_executed">>, true}],
                  [upsert]),
    ok.
