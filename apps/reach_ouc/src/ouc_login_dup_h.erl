-module(ouc_login_dup_h).
-export([init/3,
         handle/2,
         terminate/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("cowboy/include/http.hrl").
-endif.

-include("oacd_ouc.hrl").

init({_, http}, Req, _Opts) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	case cowboy_req:method(Req) of
		{<<"GET">>, Req2} ->
			handle_get(Req2, State);
		{<<"POST">>, Req2} ->
			handle_post(Req2, State);
		{_, Req2} ->
			{ok, Req3} = cowboy_req:reply(405, Req2),
			{ok, Req3, State}
	end.

handle_get(Req, State) ->
	{ok, Bin} = ouc_login_dup_dtl:render([
		{is_debug, ouc_handler_util:get_config(frontend_debug)},
		{static_root, ouc_handler_util:get_config(frontend_static_root_uri)},
		{form_action, ouc_handler_util:get_location(login_dup)}
	]),
	{ok, Req2} = cowboy_req:reply(200, [], Bin, Req),
	{ok, Req2, State}.

handle_post(Req, State) ->
	{ok, Qs, Req2} = cowboy_req:body_qs(Req),
	Force = proplists:get_value(<<"force">>, Qs),

    case Force of
		<<"1">> ->
			case ouc_session:close_existing_session(Req2) of
				{ok, Req3} ->
					{ok, Req4} = ouc_handler_util:redirect(dashboard, Req3),
					{ok, Req4, State};
				{ok, advanced_login, Req3} ->
					{ok, Req4} = ouc_handler_util:redirect(login_advanced, Req3),
					{ok, Req4, State};
				_ ->
					{ok, Req3} = ouc_session:end_session(Req2),
					{ok, Req4} = ouc_handler_util:redirect(root, Req3),
					{ok, Req4, State}
			end;
		_ ->
			{ok, Req3} = ouc_session:end_session(Req2),
			{ok, Req4} = ouc_handler_util:redirect(root, Req3),
			{ok, Req4, State}
	end.

terminate(_Reason, _Req, _State) ->
	ok.

-ifdef(TEST).
%% @todo update unit tests

mock_cowboy_modules() ->
	meck:new(cowboy_tcp_transport),
	meck:expect(cowboy_tcp_transport, send, fun(socket, _) -> ok end),
	meck:new(cowboy_clock),
	meck:expect(cowboy_clock, rfc1123, 0, <<>>).

get_login_dup_test_() ->
	{foreach, fun() ->
		mock_cowboy_modules()
	end, fun(_) ->
		meck:unload()
	end, [{"handle get", fun() ->
		Req = #http_req{method = 'GET', pid = self(), socket = socket, transport = cowboy_tcp_transport},

		?assertEqual({ok, Req#http_req{resp_state = done}, state}, handle(Req, state))
	end}, {"handle post", fun() ->
		Req = #http_req{method = 'POST', buffer = <<"force=1">>, urldecode = {[], crash}, pid = self(), socket = socket, transport = cowboy_tcp_transport},

		meck:new(cowboy_http, [passthrough]),
		meck:expect(cowboy_http, x_www_form_urlencoded, 2, [{<<"force">>, <<"1">>}]),

		meck:new(ouc_session),
		meck:expect(ouc_session, close_existing_session, 1, {ok, Req}),

		?assertEqual({ok, Req#http_req{resp_state = done}, state}, handle(Req, state)),
		?assert(meck:called(ouc_session, close_existing_session, ['_']))
	end}, {"block put", fun() ->
		Req = #http_req{method = 'PUT', pid = self(), socket = socket, transport = cowboy_tcp_transport},

		?assertEqual({ok, Req#http_req{resp_state = done}, state}, handle(Req, state)),
		?assert(meck:called(cowboy_tcp_transport, send, [socket, [<<"HTTP/1.1 405 Method Not Allowed\r\n">>, '_', '_']]))
	end}]}.

-endif.
