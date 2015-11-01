-module(ouc_root_h).
-export([init/3,
         handle/2,
         terminate/3]).

-include("oacd_ouc.hrl").

init({_, http}, Req, _Opts) ->
    {ok, Req, undefined}.

handle(Req, State) ->
	{IsLoggedIn, Req2} = is_logged_in(Req),
	RedirectTo = case IsLoggedIn of
		true -> dashboard;
		_ -> 
            lager:debug("Redirecting to login page"),
            login
	end,
	{ok, Req3} = ouc_handler_util:redirect(RedirectTo, Req2),
    lager:debug("handle - Req3: ~p",[Req3]),
	{ok, Req3, State}.

terminate(_Reason,_Req, _State) ->
    ok.

%% Internal

%% TODO check actual value of cookie
is_logged_in(Req) ->
	case cowboy_req:cookie(?CKNAME, Req) of
		{undefined, Req2} ->
            lager:debug("Cookie not found"),
			{false, Req2};
		{Cookie, Req2} ->
			lager:info("Cookie passed"),
			case ouc_session:check_cookie(Cookie, Req2) of
				{ok, Req3} ->
					lager:info("Req with set-cookie"),
					{true, Req3};
				_ ->
					{false, Req2}
			end
	end.


%% @todo add eunits
