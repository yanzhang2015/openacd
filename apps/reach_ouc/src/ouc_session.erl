-module(ouc_session).
-export([check_cookie/2, login/5, end_session/1, get_session/1, auth/1,
	handle_auth_error/1, get_logout_code/1, close_existing_session/1,
	update_existing_session/3, set_jasper_cookie/3, has_permission/2]).

-include("oacd_ouc.hrl").
-include_lib("reach_core/include/agent.hrl").
-include_lib("erlmongo/include/erlmongo.hrl").


%% -spec end_session(#http_req{}) -> #http_req{}.
end_session(Req) ->
	{Cookie, Req2} = cowboy_req:cookie(?CKNAME, Req),
	case cookie_to_session_record(Cookie) of
		{ok, Session} ->
			Username = Session#session.username,
			lager:debug("User ~p logging out", [Username]),
			Req3 = cowboy_req:set_resp_cookie(?CKNAME, <<>>, [{path, <<"/">>}], Req2),
			Req4 = cowboy_req:set_resp_cookie(?JASPER_CKNAME, <<>>, [{path, ?JASPER_CKPATH}], Req3),
            {ok, Req4};
		_ ->
			Req3 = cowboy_req:set_resp_cookie(?CKNAME, <<>>, [{path, <<"/">>}], Req2),
			Req4 = cowboy_req:set_resp_cookie(?JASPER_CKNAME, <<>>, [{path, ?JASPER_CKPATH}], Req3),
            {ok, Req4}
	end.

close_existing_session(Req) ->
	{Cookie, Req2} = cowboy_req:cookie(?CKNAME, Req),
	case cookie_to_session_record(Cookie) of
		{ok, Session} ->
			case agent_manager:query_agent(binary:bin_to_list(Session#session.username)) of
				{true, Pid} ->
					agent:stop(Pid, normal, replaced),
					case has_permission(Session#session.security_level, ?PERM_ADV_LOGIN_KEY) of
						true ->
							{ok, advanced_login, Req2};
						false ->
							{ok, Req2}
					end;
				_Other ->
					{ok, Req2}
			end;
		_ ->
			{error, invalid_cookie}
	end.

update_existing_session(Req, Force, Opts) ->
  {Cookie, Req2} = cowboy_req:cookie(?CKNAME, Req),
  case cookie_to_session_record(Cookie) of
    {ok, Session} ->
      case agent_manager:query_agent(binary:bin_to_list(Session#session.username)) of
        {true, _Pid} ->
          {error, duplicate_login, Cookie, Req2};
        _ ->
          case Force of
            <<"1">> ->
              Session1 = Session#session{options = Opts},
              {ok, NewCookie} = session_record_to_cookie(Session1),
              Req3 = cowboy_req:set_resp_cookie(?CKNAME, NewCookie, [{path, <<"/">>}], Req2),
              {ok, NewCookie, Req3};
            _ ->
              {ok, Cookie, Req2}
          end
      end;
    _ ->
      {error, invalid_cookie}
  end.


get_session(Req) ->
	{Cookie, Req2} = cowboy_req:cookie(?CKNAME, Req),
	case cookie_to_session_record(Cookie) of
		{ok, Session} ->
			{ok, Session, Req2};
		_ ->
			{error, invalid_cookie}
	end.

check_cookie(Cookie, Req) ->
	case cookie_to_session_record(Cookie) of
		{ok, Session} ->
			case agent_auth:get_agent(Session#session.username) of
				none ->
					{error, invalid_cookie};
				{ok, AgentAuth} ->
					Username = Session#session.username,
					Session2 = Session#session{last_activity = now(), security_level = AgentAuth#agent_auth.security_level},
					{ok, Cookie2} = session_record_to_cookie(Session2),
					lager:debug("Session for ~p validated, updating cookie", [Username]),
					cowboy_req:set_resp_cookie(?CKNAME, Cookie2, [{path, <<"/">>}], Req)
			end;
		_ ->
			{error, invalid_cookie}
	end.

auth(Req) ->
	{Cookie, Req2} = case cowboy_req:cookie(?CKNAME, Req) of
		{undefined, R2} ->
			{Qs, R3} = cowboy_req:qs_vals(R2),
			case proplists:get_value(<<"token">>, Qs) of
				undefined ->
					{undefined, R3};
				Token ->
					{Token, R3}
			end;
		{Ck, R2} ->
			{Ck, R2}
	end,

	case cookie_to_session_record(Cookie) of
		{ok, Session} ->
			Username = Session#session.username,

			case agent_auth:get_agent(Username) of
				none ->
					{error, invalid_cookie, []};
				{ok, AgentAuth} ->
					Session2 = Session#session{last_activity = now(), security_level = AgentAuth#agent_auth.security_level},
					{ok, Cookie2} = session_record_to_cookie(Session2),
					lager:debug("Cookie passed by websocket validated", []),
					Req3 = cowboy_req:set_resp_cookie(?CKNAME, Cookie2, [{path, <<"/">>}], Req2),

					AuthOpts = session_opts_to_auth_opts(Session#session.options),

					{ok, {Username, AuthOpts, Req3}}
			end;
		_ ->
			{error, invalid_cookie, []}
	end.

handle_auth_error(duplicate) ->
	{ok, [{redirect_location, list_to_binary(ouc_handler_util:get_location(login_dup))}]};
handle_auth_error(not_allowed) ->
  {ok, [{redirect_location, list_to_binary(ouc_handler_util:get_location(login))}]};
handle_auth_error(_) ->
	{ok, []}.

%% @todo description + update spec
%% -spec login(binary(), binary(), any(), list(), cowboy_req()) -> cowboy_req(). 
login(Username, Password, _Remember, Opts, Req) ->
		%% @todo migrate agent_auth to use binaries as well
		StrUser = binary:bin_to_list(Username),
		StrPass = binary:bin_to_list(Password),
	case agent_auth:auth(StrUser, StrPass) of
		{ok, AgentAuth} when is_record(AgentAuth, agent_auth) ->
			SLevel = AgentAuth#agent_auth.security_level,
			Session = #session{
				username = Username,
				last_login = now(),
				logout_code = random_str(10),
				security_level = SLevel,
				options = Opts
			},
			{ok, Cookie} = session_record_to_cookie(Session),
			Req2 = cowboy_req:set_resp_cookie(?CKNAME, Cookie, [{path, <<"/">>}], Req),
						lager:debug("Security level: ~p",[SLevel]),

			{ok, Req3} = case has_permission(SLevel, ?PERM_REPORTS_TAB_KEY) of
				true ->
					set_jasper_cookie(Req2, Username, Password);
				_ ->
					{ok, Req2}
			end,

			case agent_manager:query_agent(StrUser) of
				{true, _} ->
					lager:debug("Duplicate login"),
					{error, duplicate_login, Cookie, Req3};
				_ ->
					case agent_manager:are_license_seats_available() of
						true ->
							case has_permission(SLevel, ?PERM_ADV_LOGIN_KEY) of
								true ->
									{ok, advanced_login, Cookie, Req3};
								false ->
									lager:debug("Agent ~p is ok ",[StrUser]),
									{ok, Cookie, Req3}
							end;
						false ->
							{error, not_allowed}
					end
			end;
		{error, not_connected} ->
			{error, service_unavailable};
		_ ->
			{error, invalid_login}
	end.

get_logout_code(Req) ->
	{Cookie, Req2} = cowboy_req:cookie(?CKNAME, Req),
	case cookie_to_session_record(Cookie) of
		{ok, Session} ->
            lager:debug("Logout session ~p",[Session]),
			LogoutCode = Session#session.logout_code,
			{ok, LogoutCode, Req2};
		_ ->
			{error, invalid_cookie}
	end.


-spec session_record_to_cookie(#session{}) -> binary().
session_record_to_cookie(Session) ->
	CookieRaw = term_to_binary(Session),
	{ok, CookieEnc} = encrypt(CookieRaw),
	CookieB64 = binary_to_list(base64:encode(CookieEnc)),
	Cookie = list_to_binary(url_encode(CookieB64)),
	{ok, Cookie}.

-spec cookie_to_session_record(binary()) -> #session{}.
cookie_to_session_record(undefined) ->
	{error, invalid_cookie};
cookie_to_session_record(CookieEnc) ->
	CookieB64 = url_decode(binary_to_list(CookieEnc)),
	CookieDec = base64:decode(CookieB64),
	case decrypt(CookieDec) of
		{ok, Cookie} ->
			case catch binary_to_term(Cookie) of
				Term when is_record(Term, session) ->
					{ok, Term};
				_ ->
					{error, invalid_cookie}
			end;
		_ ->
			{error, decrypt_failed}
	end.

encrypt(In) ->
	{ok, Keyfile} = file:read_file(util:get_keyfile()),
	[Entry] = public_key:pem_decode(Keyfile),
	Key = public_key:pem_entry_decode(Entry),
	{ok, public_key:encrypt_public(In, Key)}.

decrypt(In) ->
	{ok, Keyfile} = file:read_file(util:get_keyfile()),
	[Entry] = public_key:pem_decode(Keyfile),
	Key = public_key:pem_entry_decode(Entry),
	try public_key:decrypt_private(In, Key) of
		Dec ->
			{ok, Dec}
	catch
		error:decrypt_failed ->
			lager:debug("Decrypt failed", []),
			{error, decrypt_failed}
	end.

url_encode(Str) ->
	edoc_lib:escape_uri(Str).

url_decode(Str) ->
	http_uri:decode(Str).

random_str(Len) ->
	Alphabet = lists:seq($A,$Z) ++ lists:seq($a,$z) ++ lists:seq($0,$9),
	lists:map(fun(_) -> Ind = crypto:rand_uniform(1, length(Alphabet)), lists:nth(Ind, Alphabet) end, lists:seq(1, Len)).

session_opts_to_auth_opts(SOpts) ->
	session_opts_to_auth_opts(SOpts, []).

session_opts_to_auth_opts([], Acc) ->
	Acc;
session_opts_to_auth_opts([{sip_uri, SipURI}|T], Acc) ->
	Endpoints = [{freeswitch_media,
		[{type,pstn},{data, SipURI}]},
	{freeswitch_voicemail,
		[{type,pstn},{data, SipURI}]},
	{freeswitch_conference,
		[{type,pstn},{data, SipURI}]}],
	session_opts_to_auth_opts(T, [{endpoints, Endpoints}|Acc]);
session_opts_to_auth_opts([_Opt|T], Acc) ->
	lager:warning("unknown session opt: ~p", [_Opt]),
	session_opts_to_auth_opts(T, Acc).

set_jasper_cookie(Req, Username, Password) ->
	Headers = [
		{"Content-Type", "application/x-www-form-urlencoded"}
	],
    JUser = "j_username=",
    JPass = "&j_password=",
    UserStr = binary:bin_to_list(Username),
    PassStr = binary:bin_to_list(Password),
	Data = lists:flatten([JUser, UserStr, JPass, PassStr]),
	ReportsUrl = reports_server_url(),
    ReportsLoginFrag = <<"/rest/login">>,
	ReportsLoginUrl = <<ReportsUrl/binary,ReportsLoginFrag/binary>>,
    ReportsLoginUrlStr = binary:bin_to_list(ReportsLoginUrl),
	Resp = http_post(ReportsLoginUrlStr, Headers, Data),

    lager:debug("POST resp: ~p",[Resp]),
	case Resp of
		{ok, "200", RespHeaders, _} ->
			case proplists:get_value("Set-Cookie", RespHeaders) of
				undefined ->
					{ok, Req};
				JasperCookie ->
					lager:info("Setting jasper cookie for ~p", [ReportsUrl]),
					Req2 = cowboy_req:set_resp_header(<<"Set-Cookie">>,
						list_to_binary(JasperCookie), Req),
                    {ok, Req2}
			end;
		_ ->
			{ok, Req}
	end.

http_post(Url, Headers, Data) ->
	ibrowse:send_req(Url, Headers, post, Data, [], 5000).

reports_server_url() ->
	case application:get_env(reach_ouc, reports_server_url) of
		undefined ->
			{ok, ConfigApi} = application:get_env(reach_ouc, sipxconfig_rest_api),
			RootUriAt = string:rstr(ConfigApi, "/sipxconfig/rest"),
			RootUri = string:sub_string(ConfigApi, 1, RootUriAt),
			binary:list_to_bin(RootUri ++ "jasperserver");
		{ok, Url} ->
			binary:list_to_bin(Url)
	end.

-spec has_permission(PermProfile :: atom(), PermKey :: binary() | tuple()) -> boolean().
has_permission(PermProfile, PermKey) ->
  {ok, Res} = agent_auth:has_permission(PermProfile, PermKey),
  Res.

%% @todo add unit tests
