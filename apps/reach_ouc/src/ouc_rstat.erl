-module(ouc_rstat).

-behaviour(gen_server).

-include("ouc_rstat.hrl").
-include_lib("reach_core/include/agent.hrl").
-include_lib("reach_core/include/call.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
	start_link/0,
	stop/0,

	get_conf/0,
	get_mn_store/0,
	get_client_rstats/0,
	get_profile_rstats/0,
	get_queue_rstats/0,
	get_agent_rstats/0,
	get_rstats/1,

	track_my_client_rstats/3,

	subscribe/0,
	unsubscribe/0,
	get_subscribers/0,

	%% prelog frontend
	subscribe_prelog/0,
	notify_logged/1,

	%% logging
	log_call/3,
	log_ustate/5
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SUBSCRIBER_KEY, {p, g, ouc_rstat_subscribe}).

-define(SNAPSHOT_GRACE_MS, 1000).
-define(MIN_REFRESH_MS, 3000).

%% for quick lookups
-define(TAB, ouc_rstat_kv).

-record(state, {
	conf,
	mn_store,
	client_rstats,
	profile_rstats,
	queue_rstats,
	agent_rstats
}).

%% API
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

get_conf() ->
	gen_server:call(?MODULE, get_conf).

get_client_rstats() ->
	get_rstats(client).

get_profile_rstats() ->
	get_rstats(profile).

get_queue_rstats() ->
	get_rstats(queue).

get_agent_rstats() ->
	get_rstats(agent).

get_rstats(Type) ->
	case quick_lookup({rstats, Type}, undefined) of
		undefined -> gen_server:call(?SERVER, {get_rstats, Type}, 60000);
		Data -> Data
	end.
get_mn_store() ->
	quick_lookup(mn_store, undefined).

track_my_client_rstats(Login, Clients, ConnPid) ->
	Pid = maybe_start_my_watcher(Login, Clients, ConnPid),
	ouc_rstat_my_watcher:get_my_rstats(Pid, Clients, ConnPid).

subscribe() ->
	%% error occurs on duplicate susbcribe
	%% catch it to supress error
	catch gproc:reg(?SUBSCRIBER_KEY),
	ok.

unsubscribe() ->
	gproc:unreg(?SUBSCRIBER_KEY).

get_subscribers() ->
	gproc:lookup_pids(?SUBSCRIBER_KEY).

%% prelog frontend

subscribe_prelog() ->
	ouc_rstat_prelogger:register().

notify_logged(Ref) ->
	ouc_rstat_prelogger:notify_logged(Ref).

%% logger

log_call(CallH, Details, Ts) ->
	Store = get_mn_store(),
	Ndxs = get_call_ndxs(Details),
	ouc_rstat_logger:log_call(Store, CallH, Ndxs, Ts).

log_ustate(UState, From, To, Details, Ts) ->
	Store = get_mn_store(),
	Ndxs = get_ustate_ndxs(Details),

	ouc_rstat_logger:log_ustate(Store, UState, From, To, Ndxs, Ts).

%% gen_server callbacks

init([]) ->
	%% prep
	RStatProps = ouc_config:get_options(reach_ouc, rstat),
    lager:debug("RStatProps: ~p",[RStatProps]),

    Backend    = ouc_config:get_property(backend, RStatProps, redis),
    lager:info("Backend: ~p", [Backend]),

	{ok, Conf} = ouc_rstat_conf:new(RStatProps),
    lager:debug("Conf : ~p",[Conf]),

    ModuleName = get_module_name(Backend),
    lager:debug("Backend module name: ~p",[ModuleName]),

    {ok, RStatStore} = ouc_rstat_store:new(ModuleName, Conf, []),
    
	%% setup quick lookup
	ets:new(?TAB, [named_table, {read_concurrency, true}]),

	quick_save(conf, Conf),
    quick_save(mn_store, RStatStore),

	%% add agent listener
	gen_event:add_handler(cpx_agent_event, ouc_agent_event_handler, []),
	ouc_gen_media_listener:register(),
	ouc_cdr_mongo:register(),

    {ok, #state{
    	conf = Conf,
        mn_store = RStatStore
    }, 0}.

handle_call(stop, _From, St) ->
	ets:delete(?TAB),
	{stop, normal, ok, St};
handle_call(get_conf, _From, St) ->
	{reply, St#state.conf, St};
handle_call({get_rstats, Type}, _From, St) ->
	Reply = case Type of
		client -> St#state.client_rstats;
		profile -> St#state.profile_rstats;
		queue -> St#state.queue_rstats;
		agent -> St#state.agent_rstats;
		_ -> undefined
	end,
	{reply, Reply, St};
handle_call(get_client_rstats, _From, St) ->
	{reply, St#state.client_rstats, St};
handle_call(get_profile_rstats, _From, St) ->
	{reply, St#state.profile_rstats, St};
handle_call(get_queue_rstats, _From, St) ->
	{reply, St#state.queue_rstats, St};
handle_call(get_agent_rstats, _From, St) ->
	{reply, St#state.agent_rstats, St};
handle_call(_Request, _From, St) ->
    Reply = ok,
    {reply, Reply, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(timeout, St) ->
	Ts = ouc_time:now(),

	St1 = init_snapshots(St, Ts),
	send_delayed_refresh(St, Ts),

	{noreply, St1};
handle_info(refresh_snapshots, St) ->
	Ts = ouc_time:now(),

	St1 = refresh_snapshots(St, Ts),
	send_delayed_refresh(St, Ts),
	gproc:send(?SUBSCRIBER_KEY, {ouc_rstat_update, Ts}),

	{noreply, St1};
handle_info(_Info, St) ->
    {noreply, St}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===========================================================================
%% Internal
%% ===========================================================================

%% ---------------------------------------------------------------------------
-spec get_module_name(atom()) -> atom().
%% @private
%% @doc Computes the module name based on a suffix and predefined prefix
%% @end
%% ---------------------------------------------------------------------------
get_module_name(Suffix) ->
    ModulePrefix = <<"ouc_rstat_store_"/utf8>>,
    ModuleSuffix = atom_to_binary(Suffix, utf8),
    BinModName = <<ModulePrefix/binary, ModuleSuffix/binary>>,
    ModuleName = binary_to_atom(BinModName, utf8),
    ModuleName.


init_snapshots(St, Ts) ->
	MnStore = St#state.mn_store,

	Clients = get_client_names(),
	Profiles = get_profile_names(),
	Queues = get_queue_names(),
	Agents = get_agent_names(),

	ClientSnap = init_snapshot(client, Clients, MnStore, Ts),
	ProfileSnap = init_snapshot(profile, Profiles, MnStore, Ts),
	QueueSnap = init_snapshot(queue, Queues, MnStore, Ts),
	AgentSnap = init_snapshot(agent, Agents, MnStore, Ts),

	quick_save({rstats, client}, ClientSnap),
	quick_save({rstats, profile}, ProfileSnap),
	quick_save({rstats, queue}, QueueSnap),
	quick_save({rstats, agent}, AgentSnap),

	St#state {
		client_rstats = ClientSnap,
		profile_rstats = ProfileSnap,
		queue_rstats = QueueSnap,
		agent_rstats = AgentSnap
	}.

init_snapshot(Type, Keys, Store, Ts) ->
	Ndxs = [[{Type, K}] || K <- Keys],
	{ok, Snap} = ouc_rstat_snapshot:new(Store, Ts, Ndxs),
	Snap.

%% TODO should be spawn'd
refresh_snapshots(St, Ts) ->
	ouc_rstat_prelogger:fire(Ts),

	ClientSnap0 = St#state.client_rstats,
	ProfileSnap0 = St#state.profile_rstats,
	QueueSnap0 = St#state.queue_rstats,
	AgentSnap0 = St#state.agent_rstats,

	Clients = get_client_names(),
	Profiles = get_profile_names(),
	Queues = get_queue_names(),
	Agents = get_agent_names(),

	ClientSnap = refresh_snapshot(client, Clients, ClientSnap0, Ts),
	ProfileSnap = refresh_snapshot(profile, Profiles, ProfileSnap0,
		Ts),
	QueueSnap = refresh_snapshot(queue, Queues, QueueSnap0, Ts),
	AgentSnap = refresh_snapshot(agent, Agents, AgentSnap0, Ts),

	quick_save({rstats, client}, ClientSnap),
	quick_save({rstats, profile}, ProfileSnap),
	quick_save({rstats, queue}, QueueSnap),
	quick_save({rstats, agent}, AgentSnap),

	St#state {
		client_rstats = ClientSnap,
		profile_rstats = ProfileSnap,
		queue_rstats = QueueSnap,
		agent_rstats = AgentSnap
	}.

refresh_snapshot(Type, Keys, OldSnap, Ts) ->
	Ndxs = [[{Type, K}] || K <- Keys],
	{ok, Snap1} = ouc_rstat_snapshot:refresh(OldSnap, [{ts, Ts},
		{indices, Ndxs}]),
	Snap1.

send_delayed_refresh(St, Ts) ->
	UpdateDelay = get_update_delay(St, Ts),
	erlang:send_after(UpdateDelay, self(), refresh_snapshots).

get_update_delay(St, Ts) ->
	Conf = St#state.conf,

	{Min, _} = ouc_rstat_conf:get_next_dynamic_update(Conf, Ts),

	UpdDelayMs = timer:seconds(Ts-Min) + ?SNAPSHOT_GRACE_MS,
	max(?MIN_REFRESH_MS, UpdDelayMs).

get_client_names() ->
	{ok, Cs} = call_queue_config:get_clients(),
	[L || #client{label=L} <- Cs].

get_profile_names() ->
	{ok, Ps} = agent_auth:get_profiles(),
	[L || #agent_profile{name=L} <- Ps].

get_queue_names() ->
	[N || {N, _} <- queue_manager:queues()].

get_agent_names() ->
	[N || {N, _} <- agent_manager:list()].

maybe_start_my_watcher(Agent, Clients, ConnPid) ->
	case ouc_rstat_my_watcher:lookup_for(Agent) of
		Pid when is_pid(Pid) ->
			Pid;
		_ ->
			MnStore = get_mn_store(),
			{ok, Pid} = ouc_rstat_my_watcher:start(Agent,
				MnStore, Clients, ConnPid),
			Pid
	end.

%% ets quick save / lookup
quick_lookup(Key, Default) ->
	case ets:lookup(?TAB, Key) of
		[{_, V}] -> V;
		_ -> Default
	end.

%% only called with rstats process
quick_save(Key, V) ->
	ets:insert(?TAB, {Key, V}).

get_call_ndxs(Details) ->
	Keys = [agent, client, queue, profile],
	Ndxs = get_ndxs(Keys, Details, []),

	Get = fun(K) -> lists:keyfind(K, 1, Details) end,
	AgentNdx = Get(agent),
	ClientNdx = Get(client),

	case is_tuple(AgentNdx) andalso is_tuple(ClientNdx) of
		true ->
			[[AgentNdx, ClientNdx]|Ndxs];
		_ ->
			Ndxs
	end.

get_ustate_ndxs(Details) ->
	Keys = [agent, profile],
	Ndxs = get_ndxs(Keys, Details, []),

	Clients = case lists:keyfind(clients, 1, Details) of
		{_, L} -> L;
		_ -> []
	end,

	ClNdxs = [[{client, Cl}] || Cl <- Clients],

	AgClNdxs =case lists:keyfind(agent, 1, Details) of
		{_, Ag} ->
			[[{agent, Ag}, {client, Cl}] || Cl <- Clients];
		_ ->
			[]
	end,

	lists:append([Ndxs, ClNdxs, AgClNdxs]).

get_ndxs([], _Details, Acc) ->
	Acc;
get_ndxs([H|T], Details, Acc) ->
	Acc1 = case lists:keyfind(H, 1, Details) of
		false ->
			Acc;
		KV ->
			[[KV]|Acc]
	end,
	get_ndxs(T, Details, Acc1).

-ifdef(TEST).
-define(M, ?MODULE).

-endif.
