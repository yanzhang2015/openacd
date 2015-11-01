-module(ouc_login_h).
-export([init/3,
         handle/2,
         terminate/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("cowboy/include/http.hrl").
-endif.

-include_lib("erlmongo/include/erlmongo.hrl").
-include_lib("reach_core/include/agent.hrl").

-define(MSG_SYSTEM_UNAVAILABLE,
	"The system is currently unavailable. Please try again later.").
-define(MSG_INVALID_LOGIN,
	"Invalid username or password.").
-define(MSG_NOT_ALLOWED,
  "The number of concurrent users exceeded license limit.").

init({_, http}, Req, _Opts) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	case cowboy_req:method(Req) of
        {<<"POST">>, Req2} ->
			handle_post(Req2, State);
        {<<"GET">>, Req2} ->
			handle_get(Req2, State);
		_ ->
			{ok, Req2} = cowboy_req:reply(405, Req),
			{ok, Req2, State}
	end.

handle_post(Req, State) ->
	{ok, Qs, Req2} = cowboy_req:body_qs(Req),
    %% @todo catch the `badlenght` answer for body_qs
	lager:debug("Login qs: ~p", [Qs]),
	Username = proplists:get_value(<<"username">>, Qs),
	Password = proplists:get_value(<<"password">>, Qs),
	Remember = proplists:get_value(<<"remember">>, Qs),

	SOpts = get_session_opts(Qs),

	case Username =/= undefined andalso Password =/= undefined of
		true ->
			case ouc_session:login(Username, Password, Remember =:= <<"on">>, SOpts, Req2) of
				{ok, Cookie, Req3} ->
					Req4 = cowboy_req:set_resp_header(<<"Content-Type">>, <<"application/json">>, Req3),
					RedirectLoc = ouc_handler_util:get_location(dashboard),
					Json = {[{redirect_location, RedirectLoc}, {token, Cookie}]},
					Bin = ejrpc2_json:encode(Json),
					{ok, Req5} = cowboy_req:reply(200, [], Bin, Req4),
					{ok, Req5, State};
				{ok, advanced_login, Cookie, Req3} ->
					Req4 = cowboy_req:set_resp_header(<<"Content-Type">>, <<"application/json">>, Req3),
					RedirectLoc = ouc_handler_util:get_location(login_advanced),
					Json = {[{redirect_location, RedirectLoc}, {token, Cookie}]},
					Bin = ejrpc2_json:encode(Json),
					{ok, Req5} = cowboy_req:reply(200, [], Bin, Req4),
					{ok, Req5, State};
				{error, duplicate_login, Cookie, Req3} ->
					Req4 = cowboy_req:set_resp_header(<<"Content-Type">>, <<"application/json">>, Req3),
					RedirectLoc = ouc_handler_util:get_location(login_dup),
					Json = {[{redirect_location, RedirectLoc},
						{token, Cookie}]},
					Bin = ejrpc2_json:encode(Json),
					{ok, Req5} = cowboy_req:reply(200, [], Bin, Req4),
					{ok, Req5, State};
				{error, not_allowed} ->
					{ok, handle_unauthorized(Req, [
						{error, ?MSG_NOT_ALLOWED}], 403), State};
				{error, service_unavailable} ->
					{ok, handle_unauthorized(Req, [
						{error, ?MSG_SYSTEM_UNAVAILABLE}], 503), State};
				_ ->
					{ok, handle_unauthorized(Req, [
						{error, ?MSG_INVALID_LOGIN}], 401), State}
			end;
		false ->
			{ok, handle_unauthorized(Req, []), State}
	end.

handle_get(Req, State) ->
	{Qs, Req2} = cowboy_req:qs_vals(Req),
    case proplists:get_value(<<"err">>, Qs) of
    	undefined ->
    		{ok, handle_unauthorized(Req2, []), State};
    	Err ->
    		{_, Message} = ouc_error:err({code, Err}),
    		{ok, handle_unauthorized(Req2, [{error, Message}]), State}
	end.

handle_unauthorized(Req, RenderOpts) ->
	handle_unauthorized(Req, RenderOpts, 200).

handle_unauthorized(Req, RenderOpts, Code) ->
	{ok, Bin} = ouc_login_dtl:render(RenderOpts ++ [
			{is_debug, ouc_handler_util:get_config(frontend_debug)},
			{static_root, ouc_handler_util:get_config(frontend_static_root_uri)}
		]),
	{ok, Req2} = cowboy_req:reply(Code, [], Bin, Req),
	Req2.

terminate(_Reason, _Req, _State) ->
	ok.

%% @todo migrate to binaries
get_session_opts(Qs) ->
	get_session_opts(Qs, []).

get_session_opts([], Acc) ->
	Acc;
get_session_opts([{<<"sip_uri">>, AddrBin}|T], Acc) when AddrBin =/= <<>> ->
	Addr = binary_to_list(AddrBin),
	"sip:" ++ Addr2 = freeswitch_media_manager:fix_sip_addr(Addr),

	Entry = {sip_uri, Addr2},
	get_session_opts(T, [Entry|Acc]);
get_session_opts([_|T], Acc) ->
	get_session_opts(T, Acc).

-ifdef(TEST).
%% @todo update tests

mock_cowboy_modules() ->
	meck:new(cowboy_tcp_transport),
	meck:expect(cowboy_tcp_transport, send, fun(socket, _) -> ok end),
	meck:new(cowboy_clock),
	meck:expect(cowboy_clock, rfc1123, 0, <<>>).

t_get_req() ->
	#http_req{method = 'GET', pid = self(),
		socket = socket, transport = cowboy_tcp_transport,
		urldecode = {[], crash}}.

t_post_req() ->
	#http_req{method = 'POST', pid = self(),
		socket = socket, transport = cowboy_tcp_transport,
		urldecode = {[], crash}}.

t_urlencoded_data(Data) ->
	meck:expect(cowboy_http, x_www_form_urlencoded, 2, Data).

t_assert_redirect(Handler) ->
	Location = ouc_handler_util:get_location(Handler),
		?assert(meck:called(cowboy_tcp_transport, send, [socket,
			[<<"HTTP/1.1 303 See Other\r\n">>,
			[[<<"Location">>,<<": ">>,Location,<<"\r\n">>]|'_'], '_']])).

t_assert_response(401) ->
	?assert(meck:called(cowboy_tcp_transport, send, [socket,
			[<<"HTTP/1.1 401 Unauthorized\r\n">>,'_', '_']]));
t_assert_response(200) ->
	?assert(meck:called(cowboy_tcp_transport, send, [socket,
			[<<"HTTP/1.1 200 OK\r\n">>, '_', '_']]));
t_assert_response(503) ->
	?assert(meck:called(cowboy_tcp_transport, send, [socket,
			[<<"HTTP/1.1 503 Service Unavailable\r\n">>,'_', '_']])).

t_assert_rendered(Template, Opts) ->
	{ok, Bin} = Template:render(Opts ++ [{is_debug, false}, {static_root, ""}]),
	?assert(meck:called(cowboy_tcp_transport, send, [socket, Bin])).

handle_req_test_() ->
	{foreach, fun() ->
		agent_auth_ets:setup([{agents, ["agent"]}]),
		meck:new(cowboy_http, [passthrough]),
		meck:new(ouc_session),
		meck:expect(ouc_session, login,
			fun("agent","password",_,Req) -> {ok, Req};
				(_,_,_,_) -> {error, invalid_login}
			end),
		mock_cowboy_modules()
	end, fun(_) ->
		meck:unload()
	end, [
	{"valid login, redirect to dashboard", fun() ->
		t_urlencoded_data([{<<"username">>,<<"agent">>},
			{<<"password">>, <<"password">>}]),

		?assertMatch({ok, #http_req{resp_state=done}, st},
			handle(t_post_req(), st)),
		t_assert_redirect(dashboard)
	end},
	{"invalid login, return 401", fun() ->
		t_urlencoded_data([{<<"username">>, <<"agent">>},
			{<<"password">>, <<"wrongpass">>}]),

		?assertMatch({ok, #http_req{resp_state=done}, st},
			handle(t_post_req(), st)),

		t_assert_response(401),
		t_assert_rendered(ouc_login_dtl,
			[{error, ?MSG_INVALID_LOGIN}])
	end},
	{"get login page", fun() ->
		t_urlencoded_data([]),
		?assertMatch({ok, #http_req{resp_state=done}, st},
			handle(t_get_req(), st)),

		t_assert_response(200),
		t_assert_rendered(ouc_login_dtl, [])
	end},
	{"auth service unavailable, return 503", fun() ->
		meck:expect(ouc_session, login, 4, {error, service_unavailable}),
		t_urlencoded_data([{<<"username">>, <<"agent">>},
			{<<"password">>, <<"password">>}]),

		?assertMatch({ok, #http_req{resp_state=done}, st},
			handle(t_post_req(), st)),

		t_assert_response(503),
		t_assert_rendered(ouc_login_dtl,
			[{error, ?MSG_SYSTEM_UNAVAILABLE}])
	end}]}.

-endif.
