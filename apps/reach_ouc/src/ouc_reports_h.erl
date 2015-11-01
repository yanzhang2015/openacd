-module(ouc_reports_h).
-export([init/3,
         handle/2,
         terminate/3]).

-include("oacd_ouc.hrl").

-define(PINTOKEN_KEY, {"pntk"}).

init({_, http}, Req, _Opts) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	case cowboy_req:method(Req) of
		{<<"POST">>, Req2} ->
			handle_post(Req2, State);
		{_, Req2} ->
			handle_method_not_allowed(Req2, State)
	end.

handle_post(Req, State) ->
	case ouc_session:get_session(Req) of
		{ok, #session{security_level = SLevel, username = Username} = _Session, Req2} ->
			case ouc_session:has_permission(SLevel, ?PERM_REPORTS_TAB_KEY) of
				true ->
					Password = get_user_password(Username),
					{ok, Req3} = ouc_session:set_jasper_cookie(Req2, Username, Password),
					{ok, Req3, State};
				_ ->
					handle_unauthorized(Req, State)
			end;
		_ ->
			handle_unauthorized(Req, State)
	end.

terminate(_Reason, _Req, _State) ->
	ok.

handle_method_not_allowed(Req, State) ->
	{ok, Req2} = cowboy_req:reply(405, Req),
	{ok, Req2, State}.

handle_unauthorized(Req, State) ->
	{ok, Req2} = cowboy_req:reply(401, Req),
	{ok, Req2, State}.

get_user_password(User) ->
	Db = ouc_db:get_db(imdb),
	{ok, Props} = Db:findOne(<<"entity">>, [{<<"type">>, <<"openacdagent">>}, {<<"name">>, User}]),
	Get = fun(Q) -> ej:get(Q, Props) end,
	Passwd = Get(?PINTOKEN_KEY),
	Passwd.

%% @todo add eunit tests
