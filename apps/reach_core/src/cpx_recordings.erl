-module(cpx_recordings).

-include("call.hrl").
-include("queue.hrl").

-include_lib("reach_ouc/include/ouc_recordings.hrl").

-export([get_path/4,
         get_path/3,
         is_recording_enabled/1,
         store_file/3,
         get_recording_format/0]).

-import(cpx_json_util, [l2b/1, b2l/1, nob/1, nol/1]).

-define(OUTBOUND_LINE, "Outbound").

get_path(CallId, InitTime, ClientId, AllowDisabled) ->
	case call_queue_config:get_client_by_id(ClientId) of
		{ok, Client} ->
			ClientOpts = Client#client.options,
			Enabled = proplists:get_value(recording_enabled, ClientOpts, false),
			Dir = proplists:get_value(recording_dir, ClientOpts),
			case Dir of
				undefined ->
					undefined;
				_ ->
					case Enabled orelse AllowDisabled of
						true ->
							{{Y,M,D}, {H,Mi,S}} = calendar:now_to_local_time(InitTime),
							DateIo = io_lib:format("~B-~2..0B-~2..0B", [Y,M,D]),
							TimeIo = io_lib:format("~2..0B~2..0B~2..0B", [H,Mi,S]),
                            StrDir = string:strip(Dir, right, $/),
							lists:flatten(io_lib:format("~s/~s/~s/~s-~s.~s",
							[StrDir, ClientId, DateIo, TimeIo, CallId, get_recording_ext()]));
						_ ->
							undefined
					end
			end;
		_ ->
			undefined
	end.

get_path(CallId, InitTime, ClientId) ->
	get_path(CallId, InitTime, ClientId, false).


is_recording_enabled(ClientId) ->
	case call_queue_config:get_client_by_id(ClientId) of
		{ok, Client} ->
			ClientOpts = Client#client.options,
			case proplists:get_value(recording_enabled, ClientOpts) of
				true ->
					true;
				_ ->
					false
			end;
		_ ->
			false
	end.

%% --------------------------------------------------------------------
-spec(store_info/5 :: (Call :: #call{},
                       Timestamps :: list(),
                       AgentLogin :: list(),
                       RecordingPath :: list(),
                       IsFileUploaded :: boolean()) -> true | false).
%% @doc Stores file metadata corresponding to a given call recording
%%  in Mongo.
%% @end
%% --------------------------------------------------------------------

store_info(Call, Timestamps, AgentLogin, RecordingPath, IsFileUploaded) ->
	#call{
		id = Id,
		callerid = CallerIdTuple,
		skills = Skills,
		client = Client,
		queue = Queue,
		dnis = Dnis
	} = Call,

	StartTimestamp = proplists:get_value(init, Timestamps),
	OncallTimestamp = lists:min(proplists:get_all_values(oncall, Timestamps)),
	WrapupTimestamp = lists:max(proplists:get_all_values(wrapup, Timestamps)),
	StartMs = util:now_ms(StartTimestamp),
	OncallMs = util:now_ms(OncallTimestamp),
	WrapupMs = util:now_ms(WrapupTimestamp),
	{CallerId, CallerAni} = CallerIdTuple,
	ClientId = Client#client.id,
	ClientLabel = Client#client.label,

	Line = case Queue of
	  ?OUTBOUND_QUEUE -> ?OUTBOUND_LINE;
	  _ -> get_line_name(Dnis)
	end,

	RecordingLoc = "/recordings?call_id=" ++ Id ++ "&client=" ++ ClientId ++
								 "&init_time=" ++ integer_to_list(StartMs),
	lager:info("Recording to be served at ~p", [RecordingLoc]),

	Entry = [
		{<<"call_id">>, l2b(Id)},
		{<<"start_timestamp">>, StartTimestamp},
		{<<"caller_ani">>, nob(CallerAni)},
		{<<"caller_id">>, nob(CallerId)},
		{<<"skills">>, enc_skills(Skills)},
		{<<"client_id">>, l2b(ClientId)},
		{<<"client">>, l2b(ClientLabel)},
		{<<"queue">>, nob(Queue)},
		{<<"line">>, nob(Line)},
		{<<"agent">>, nob(AgentLogin)},
		{<<"recording_location">>, l2b(RecordingLoc)},
		{<<"recording_ms">>, WrapupMs - OncallMs},
		{<<"filename">>, l2b(RecordingPath)},
		{<<"recording_file_type">>, get_recording_binext()}
	],

	lager:info("Saving metadata for recording file corresponding to call ~p, uploaded ~p", [Id, IsFileUploaded]),
	Db = ouc_db:get_db(reach),
	case IsFileUploaded of
		true ->
			Db:save(binary_to_list(?REC_META_COLL), Entry ++ [{<<"db_uploaded">>, <<"true">>}]);
		false ->
			Db:save(binary_to_list(?REC_META_COLL), Entry)
	end,
	ok.


%% ---------------------------------------------------------------------------------
-spec(store_file/3 :: (Call :: #call{},
                       Timestamps :: list(),
                       AgentLogin :: list()) -> true | false).
%% @doc Spawns a new process that stores the file in Mongo.
%% @end
%% --------------------------------------------------------------------------------
store_file(Call, Timestamps, AgentLogin) ->
	Fun = fun() ->
		upload_file(Call, Timestamps, AgentLogin)
	end,
	spawn(Fun).


%% ---------------------------------------------------------------------------------
-spec(upload_file/3 :: (Call :: #call{},
                       Timestamps :: list(),
                       AgentLogin :: list()) -> true | false).
%% @doc Uploads file recording in Mongo using GridFS support.
%%  Each file is splitted in equal chunks and each of those chunks is stored as a
%%  separate document in fs.chunks collection.
%%  Metadata about the file is automatically saved by GridFS in fs.files collection.
%% @end
%% --------------------------------------------------------------------------------
upload_file(Call, Timestamps, AgentLogin) ->
	timer:sleep(?REC_UPLOAD_DELAY),
	ClientId = Call#call.client#client.id,
	CallId = Call#call.id,
	RecEnabled = is_recording_enabled(ClientId),

	lager:debug("Timestamps of the call in recordings module: ~p", [Timestamps]),
	case RecEnabled of
		true ->
			StartTimestamp = proplists:get_value(init, Timestamps),
			Db = ouc_db:get_db(reach),
			RecordingPath = get_path(CallId, StartTimestamp, ClientId),

			case file:read_file(RecordingPath) of
				{ok,  BinFile} ->
					Pid = Db:gfsNew(?REC_GFS_BUCKET, RecordingPath, [{chunkSize, ?REC_CHUNK_SIZE}]),
					lager:info("Saving new entry for call id ~p at timestamp ~p", [CallId, util:now_ms(StartTimestamp)]),
					Db:gfsWrite(Pid, BinFile),
					Db:gfsClose(Pid),
					store_info(Call, Timestamps, AgentLogin, RecordingPath, true),
					true;

				{error, Reason} ->
					lager:error("Error while trying to save the recording ~p in Mongo: ~p", [RecordingPath, Reason]),
					store_info(Call, Timestamps, AgentLogin, RecordingPath, false),
					false
			end;

		_ ->
			lager:info("The call recording option is not enabled for the client ~p", [ClientId]),
			false
	end.


enc_skills(Skills) ->
	{array, lists:reverse(lists:foldl(
		fun(At, Acc) when is_atom(At) -> [atom_to_binary(At, utf8)|Acc];
			% ({'_agent', Agent}, Acc) -> [{<<"agent">>, l2b(Agent)}|Acc];
			% ({'_brand', Client}, Acc) -> [{<<"client">>, l2b(Client)}|Acc];
			% ({'_node', Node}, Acc) -> [{<<"node">>, atom_to_binary(Node, utf8)}|Acc];
			% ({'_profile', Profile}, Acc) -> [{<<"profile">>, l2b(Profile)}|Acc];
			% ({'_queue', Queue}, Acc) -> [{<<"queue">>, l2b(Queue)}|Acc];
			(_, Acc) -> Acc
		end, [], Skills))}.


get_line_name(Line) ->
	Db = ouc_db:get_db(imdb),
	{ok, Props} = Db:findOne(<<"entity">>, [{<<"ent">>, <<"openacdline">>}, {<<"ident">>, {regex, "^" ++ Line ++ "@.*", "i"}}]),
	case ej:get({"linm"}, Props, null) of
		null ->
			lager:info("No DB entry found for line ~p", [Line]),
			undefined;
		N ->
			b2l(N)
	end.

%% ----------------------------------------------------------------------------------
-spec get_recording_format() -> atom().
%% @doc Returns the recording format configured in sys.config
%%      By default this is mp3. In prior versions (14.04 and older) this will be wav.
%% @end
%% ----------------------------------------------------------------------------------
get_recording_format() ->
    RecordingFormat = case ouc_config:get_options(reach_core, recording_format) of
        [] -> mp3;
        Value -> Value
    end,
    RecordingFormat.

%% =================================================================
%% private functions
%% =================================================================
get_recording_ext() ->
    atom_to_list(get_recording_format()).

get_recording_binext() ->
    atom_to_binary(get_recording_format(),utf8).
