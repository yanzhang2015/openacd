-module(ouc_call_recordings_h).
-export([init/3,
         handle/2,
         terminate/3]).

-import(cpx_json_util, [l2b/1, b2l/1, nob/1, nol/1]).

-include("oacd_ouc.hrl").

-include("ouc_recordings.hrl").

-include_lib("erlmongo/include/erlmongo.hrl").

%% Internal state for ouc_call_recordings_h
-record(int_state, {
		format::binary()
}).

init({_, http}, Req, _Opts) ->
    State = #int_state{},
    {ok, Req, State}.

handle(Req, State) ->
	case cowboy_req:method(Req) of
		{<<"HEAD">>, Req2} ->
			handle_get(Req2, State);
		{<<"GET">>, Req2} ->
			handle_get(Req2, State);
		_ ->
			handle_method_not_allowed(Req, State)
	end.


handle_get(Req, State) ->
	case ouc_session:get_session(Req) of
		{ok, #session{security_level = SLevel} = _Session, Req2} ->
			case ouc_session:has_permission(SLevel, ?PERM_CALL_REC_KEY) of
				true ->
					{Qs, Req3} = cowboy_req:qs_vals(Req2),

					ClientBin = proplists:get_value(<<"client">>, Qs),
					InitBin = proplists:get_value(<<"init_time">>, Qs),
					CallIdBin = proplists:get_value(<<"call_id">>, Qs),

					lager:info("Getting recordings: ~p ~p ~p", [ClientBin, InitBin, CallIdBin]),
					case lists:member(undefined, [ClientBin, InitBin, CallIdBin]) of
						false ->

							%% Read file from Mongo
							Db = ouc_db:get_db(reach),
							{ok, FileInfo} = Db:findOne(?REC_META_COLL,
											[{<<"call_id">>, CallIdBin},
											{<<"client_id">>, ClientBin}]),
							RecordingFileName = ej:get({"filename"}, FileInfo, undefined),

							case RecordingFileName of
								undefined ->
									handle_not_found(Req3, State);
								_ ->
									{ok, FileMeta} = Db:findOne(<< ?REC_GFS_BUCKET/binary, <<".">>/binary, ?REC_GFS_FILES_COLL/binary >>,
												[{<<"filename">>, RecordingFileName}]),
									FileLength = ej:get({"length"}, FileMeta, 0),
									Pid = mongoapi:gfsOpen(?REC_GFS_BUCKET, #gfs_file{filename = RecordingFileName}, Db),
									Bin = mongoapi:gfsRead(Pid, FileLength, Db),
									mongoapi:gfsClose(Pid, Db),
									lager:debug("The file ~p of length ~p was read from GridFS. Read Bin size: ~p",[RecordingFileName, FileLength, byte_size(Bin)]),

									{Headers, Req4} = cowboy_req:headers(Req3),
									lager:debug("Request Headers: ~p", [Headers]),
									RangeData = proplists:get_value(<<"range">>, Headers),
									RecFormat = ej:get({"recording_file_type"}, FileInfo, <<"wav">>),
									ContentType = case RecFormat of
															<<"mp3">> -> <<"audio/mpeg">>;
															<<"wav">> -> <<"audio/x-wav">>;
															_         -> <<"audio/mpeg">>
									end,
									NewState = State#int_state{format = ContentType},

									case RangeData of
										undefined ->
											lager:info("Full request for file ~p", [RecordingFileName]),
											handle_full_request(Bin, Req4, NewState);
										_ ->
											lager:info("Partial request for file ~p, range ~p", [RecordingFileName, RangeData]),
											handle_range_request(Bin, RangeData, Req4, NewState)
									end
							end;
						true ->
							handle_bad_request(Req2, State)
					end;
				false ->
					handle_unauthorized(Req, State)
			end;
		_ ->
			handle_unauthorized(Req, State)
	end.

handle_method_not_allowed(Req, State) ->
    {ok, Req2} = cowboy_req:reply(405, Req),
	{ok, Req2, State}.

handle_bad_request(Req, State) ->
    {ok, Req2} = cowboy_req:reply(400, Req),
	{ok, Req2, State}.

handle_not_found(Req, State) ->
	{ok, Req2} = cowboy_req:reply(404, Req),
	{ok, Req2, State}.

handle_unauthorized(Req, State) ->
	{ok, Req2} = cowboy_req:reply(401, Req),
	{ok, Req2, State}.

handle_full_request(Bin, Req, State) ->
	ContentType = State#int_state.format,
    lager:debug("ContentType of recording is: ~p",[ContentType]),
	Req2 = cowboy_req:set_resp_header(<<"Content-Type">>, ContentType, Req),
	{ok, Req3} = cowboy_req:reply(200, [], Bin, Req2),
	{ok, Req3, State}.

handle_range_request(Bin, RangeData, Req, State) ->
	Range = ouc_cowboy_util:range(RangeData),
	{PartialBin, BinRange} = return_file_range(Range, Bin),
	ContentType = State#int_state.format,
    lager:debug("ContentType of recording is: ~p",[ContentType]),

	Req2 = cowboy_req:set_resp_header(<<"Accept-Ranges">>, <<"bytes">>, Req),
	Req3 = cowboy_req:set_resp_header(<<"Content-Range">>, list_to_binary(BinRange), Req2),
	Req4 = cowboy_req:set_resp_header(<<"Content-Type">>, ContentType, Req3),
	{ok, Req5} = cowboy_req:reply(206, [], PartialBin, Req4),
	{ok, Req5, State}.

return_file_range({<<"bytes">>, List}, Bin) ->
	return_file_range(List, Bin);

return_file_range([{0, infinity}], Bin) ->
	ByteSize = erlang:byte_size(Bin),
	BinRange = "bytes 0-" ++ integer_to_list(ByteSize-1) ++ "/" ++ integer_to_list(ByteSize),
	{Bin, BinRange};
return_file_range([{M, infinity}], Bin) ->
	ByteSize = erlang:byte_size(Bin),
	BinRange = "bytes " ++ integer_to_list(M) ++ "-" ++ integer_to_list(ByteSize-1) ++ "/" ++ integer_to_list(ByteSize),

	Before = M,
	<<_:Before/bytes, Keep/binary>> = Bin,
	{Keep, BinRange};
return_file_range([{0, N}], Bin) ->
	ByteSize = erlang:byte_size(Bin),
	BinRange = "bytes 0-" ++ integer_to_list(N) ++ "/" ++ integer_to_list(ByteSize),

	Until = N+1,
	<<Keep:Until/bytes, _/binary>> = Bin,
	{Keep, BinRange};
return_file_range([{M, N}], Bin) ->
	ByteSize = erlang:byte_size(Bin),
	BinRange = "bytes " ++ integer_to_list(M) ++ "-" ++ integer_to_list(N) ++ "/" ++ integer_to_list(ByteSize),

	Before = M,
	Mid = N-M+1,
	<<_:Before/bytes, Keep:Mid/bytes, _/binary>> = Bin,
	{Keep, BinRange};

return_file_range(_, Bin) ->
	return_file_range([{0, infinity}], Bin).

terminate(_Reason, _Req, _State) ->
	ok.

%% @todo add unit tests
