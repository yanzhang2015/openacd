-module(ouc_web).
-author("jvliwanag").

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("oacd_ouc.hrl").

%% api
-export([start/0, start/1, start_link/0, start_link/1, stop/0]).

-record(state, {https_pid:: pid(), http_pid:: pid()}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(DISPATCH,
[{'_', [
	{[<<"/wsock">>], ouc_agent_wsock_handler,
		[{rpc_mods, [
			{ouc_rpc, [{prefix, <<"ouc">>}]},
			%%{ouc_sup_rpc, [{prefix, <<"ouc_sup">>},
			%%	{security_level, supervisor}]}]},
			{ouc_sup_rpc, [{prefix, <<"ouc_sup">>}]}]},
		{info_handlers, [{ouc_wsock_handler, handle_wsinfo}]}]},
	{[<<"/">>], ouc_root_h,
		[]},
	{[<<"/logout">>], ouc_logout_h,
		[]},
	{[<<"/login">>], ouc_login_h,
		[]},
	{[<<"/login/duplicate">>], ouc_login_dup_h,
		[]},
	{[<<"/login/advanced">>], ouc_login_advanced_h,
		[]},
	{[<<"/dashboard">>], ouc_dashboard_h,
		[{tabs, [
			{<<"main">>, [{gadgets, [
				{<<"agent state">>, [{position, {1, 1}}, {width, 1}]},
				{<<"session manager">>, [{position, {2, 1}}, {width, 2}]},
				{<<"my statistics">>, [{position, {2, 10}}, {width, 2}]}
			]}, {layout, narrow}]},
			{<<"supervisor">>, [{gadgets, [
				{<<"queue manager">>, [{position, {1, 1}}, {width, 3}]},
				{<<"agent manager">>, [{position, {1, 20}}, {width, 3}]}
      ]}, {layout, narrow}, {security_level, supervisor}]},
      {<<"reports">>, [{layout, 'no-grid'}, {security_level, supervisor}]}]}]},
    {[<<"/recordings">>], ouc_call_recordings_h,
		[]},
	{[<<"/reports/auth">>], ouc_reports_h,
		[]}
    ]}]
).
-define(HTTPS_LISTENER, reach_ouc_https).
-define(HTTP_LISTENER, reach_ouc_http).

%% @doc Starts the web listener on the default port of 5055.
-spec(start/0 :: () -> {'ok', pid()}).
start() ->
	start([]).

%% @doc Starts the web listener on the passed port.
-spec(start/1 :: (Port :: non_neg_integer()) -> {'ok', pid()}).
start(Port) when is_integer(Port) ->
	start([{port, Port}]);
start(Options) ->
	gen_server:start({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Start linked on the default port of 5055.
-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
	start_link([]).

%% @doc Start linked on the given port.
-spec(start_link/1 :: (Port :: non_neg_integer()) -> {'ok', pid()}).
start_link(Port) when is_integer(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []);
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Stop the web listener.
-spec(stop/0 :: () -> 'ok').
stop() ->
	gen_server:call(?MODULE, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(_Options) ->
	process_flag(trap_exit,true),

    OucWebOpts = ouc_config:get_options(reach_ouc, ouc_web),
    HttpsPort  = ouc_config:get_property(ws_port, OucWebOpts, ?DEFAULT_PORT),
    HttpPort   = HttpsPort + 1,

    CertFile = proplists:get_value(ssl_certfile, OucWebOpts),
    KeyFile  = proplists:get_value(ssl_keyfile, OucWebOpts),
    TotalCowboyWorkers = proplists:get_value(cowboy_workers, OucWebOpts, 100),
    CowboyWorkers = TotalCowboyWorkers div 2,
    SSLVers = proplists:get_value(cowboy_ssl_versions, OucWebOpts, [tlsv1] ),

    lager:debug("WebSocket bind port: ~p",[HttpsPort]),
    lager:debug("Total cowboy workers: ~p",[TotalCowboyWorkers]),
    lager:debug("Cowboy workers for wss or ws: ~p",[CowboyWorkers]),
    lager:debug("Cowboy ssl versions: ~p",[SSLVers]),

    CowboyDispatch = cowboy_router:compile(?DISPATCH),
    ProtoOptions = [{env, [{dispatch, CowboyDispatch}]}],
    WssOptions  = [
			{port, HttpsPort},
			{certfile, CertFile},
			{keyfile,  KeyFile},
            {versions, SSLVers}
    ],
    WsOptions = [{port, HttpPort}],
	{ok, HttpsPid} = cowboy:start_https(?HTTPS_LISTENER, CowboyWorkers,
                                        WssOptions, ProtoOptions),

	{ok, HttpPid} = cowboy:start_http(?HTTP_LISTENER, CowboyWorkers,
                                      WsOptions, ProtoOptions),

	cpx_hooks:set_hook(ouc_web_auth, wsock_auth, {ouc_session, auth, []}),
	cpx_hooks:set_hook(ouc_web_auth_error, wsock_auth_error, {ouc_session, handle_auth_error, []}),
	{ok, #state{https_pid = HttpsPid, http_pid = HttpPid}}.

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(Request, _From, State) ->
    {reply, {unknown_call, Request}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(Reason, #state{}) ->
	lager:notice("stopping web listener: ~p", [Reason]),
	catch cowboy:stop_listener(?HTTPS_LISTENER),
	catch cowboy:stop_listener(?HTTP_LISTENER),
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @todo add unit tests.
