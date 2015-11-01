-module(ouc_login_advanced_h).
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
  %% @todo catch the `badlenght` answer for body_qs
  {ok, Bin} = ouc_login_advanced_dtl:render([
    {is_debug, ouc_handler_util:get_config(frontend_debug)},
    {static_root, ouc_handler_util:get_config(frontend_static_root_uri)},
    {form_action, ouc_handler_util:get_location(login_advanced)}
  ]),
  {ok, Req2} = cowboy_req:reply(200, [], Bin, Req),
  {ok, Req2, State}.

handle_post(Req, State) ->
  {ok, Qs, Req2} = cowboy_req:body_qs(Req),
  Force = proplists:get_value(<<"force">>, Qs),
  SOpts = get_session_opts(Qs),

  case ouc_session:update_existing_session(Req2, Force, SOpts) of
    {ok, Cookie, Req3} ->
      lager:info("Updated existing session ok, redirecting to the dashboard!", []),
      Req4 = cowboy_req:set_resp_header(<<"Content-Type">>, <<"application/json">>, Req3),
      RedirectLoc = ouc_handler_util:get_location(dashboard),
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
    _ ->
      {ok, Req3} = ouc_session:end_session(Req2),
      {ok, Req4} = ouc_handler_util:redirect(root, Req3),
      {ok, Req4, State}
  end.

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