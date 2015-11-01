-module(ouc_handler_util).
-export([redirect/2, get_config/1, get_location/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec redirect(atom(), cowboy_req:req()) -> {ok, cowboy_req:req()}.
redirect(Handler, Req) ->
	Req2 = cowboy_req:set_resp_header(<<"Location">>, get_location(Handler), Req),
	cowboy_req:reply(303, Req2).

get_config(frontend_debug) ->
	case application:get_env(reach_ouc, frontend_debug) of
		undefined ->
			false;
		{ok, V} ->
			V
	end;

%% @todo - verify if still relevant - most likely deprecated.
get_config(frontend_static_root_uri)->
	case application:get_env(reach_ouc, frontend_static_root_uri) of
		undefined ->
			"";
		{ok, V} ->
			list_to_binary(V)
	end;
get_config(root_uri)->
	case application:get_env(reach_ouc, root_uri) of
		undefined ->
			<<"">>;
		{ok, V} ->
			list_to_binary(V)
	end.

get_location(Handler) ->
    RootUri = get_config(root_uri),
    RelativePath = get_relative_path(Handler),
    <<RootUri/binary, RelativePath/binary>>.

% Internal functions

get_relative_path(dashboard) ->
    <<"/dashboard">>;
get_relative_path(login) ->
    <<"/login">>;
get_relative_path(login_dup) ->
    <<"/login/duplicate">>;
get_relative_path(login_advanced) ->
	<<"/login/advanced">>;
get_relative_path(logout) ->
    <<"/logout">>;
get_relative_path(root) ->
    <<"/">>.

% Eunit

-ifdef(TEST).
% @todo update unit tests

mock_cowboy_modules() ->
	meck:new(cowboy_tcp_transport),
	meck:expect(cowboy_tcp_transport, send, fun(socket, _) -> ok end),
	meck:new(cowboy_clock),
	meck:expect(cowboy_clock, rfc1123, 0, <<>>).

assert_redirect_to(Location) ->
	LocBin = [<<"Location">>,<<": ">>,Location,<<"\r\n">>],
	[{_, {cowboy_tcp_transport, send, [socket, [<<"HTTP/1.1 303 See Other\r\n">>, Bin, <<"\r\n">>]]}, ok} | _] = meck:history(cowboy_tcp_transport),
	[FirstBin|_] = Bin,
	?assertEqual(LocBin, FirstBin).

redirect_test_() ->
	Req = #http_req{method = 'GET', pid = self(), socket = socket, transport = cowboy_tcp_transport},
	{foreach, fun() ->
		mock_cowboy_modules(),
		application:set_env(reach_ouc, root_uri, "/openacd")
	end, fun(_) ->
		application:unset_env(reach_ouc, root_uri),
		meck:unload()
	end, [{"redirect to /login", fun() ->
		?assertEqual({ok, Req#http_req{resp_state=done}}, redirect(login, Req)),
		assert_redirect_to("/openacd/login")
	end}, {"redirect to /login/duplicate", fun() ->
		?assertEqual({ok, Req#http_req{resp_state=done}}, redirect(login_dup, Req)),
		assert_redirect_to("/openacd/login/duplicate")
	end}, {"redirect to /logout", fun() ->
		?assertEqual({ok, Req#http_req{resp_state=done}}, redirect(logout, Req)),
		assert_redirect_to("/openacd/logout")
	end}, {"redirect to root", fun() ->
		?assertEqual({ok, Req#http_req{resp_state=done}}, redirect(root, Req)),
		assert_redirect_to("/openacd/")
	end}]}.

get_config_test_() ->
	{setup, fun() ->
		application:set_env(reach_ouc, frontend_debug, true),
		application:set_env(reach_ouc, frontend_static_root_uri, "/static")
	end, fun(_) ->
		application:unset_env(reach_ouc, frontend_debug),
		application:unset_env(reach_ouc, frontend_static_root_uri)
	end, [
		?_assertEqual(true, get_config(frontend_debug)),
		?_assertEqual("/static", get_config(frontend_static_root_uri))
	]}.

-endif.
