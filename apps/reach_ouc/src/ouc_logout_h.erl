-module(ouc_logout_h).
-export([init/3,
         handle/2,
         terminate/3]).

init({_, http}, Req, _Opts) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Qs, Req2} = cowboy_req:qs_vals(Req),
    case proplists:get_value(<<"c">>, Qs) of
        undefined ->
            lager:debug("No c value in logout qs"),
            {ok, Req2} = ouc_handler_util:redirect(dashboard, Req),
            {ok, Req2, State};
        Code ->
            case ouc_session:get_logout_code(Req2) of
                {ok, SessionCodeStr, Req3} ->
                    SessionCode = list_to_binary(SessionCodeStr),
                    lager:debug("Comparing session logout code ~p to ~p", [SessionCode, Code]),
                    case SessionCode =:= Code of
                        true ->
                            {ok, Req4} = ouc_session:end_session(Req3),
                            {ok, Req5} = ouc_handler_util:redirect(login, Req4),
                            {ok, Req5, State};
                        false ->
                            {ok, Req31} = ouc_handler_util:redirect(dashboard, Req3),
                            {ok, Req31, State}
                    end;
                _ ->
                    lager:debug("Error in retrieving logout code from session"),
                    {ok, Req21} = ouc_handler_util:redirect(dashboard, Req2),
                    {ok, Req21, State}
                end
        end.

terminate(_Reason, _Req, _State) ->
    ok.

%% @todo add/update unittests
