-module(ouc_rstat_my_watcher).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ouc_rstat.hrl").

%% API
-export([
	lookup_for/1,

	start/4,
	stop/1,

	get_snapshot/1,
	get_my_rstats/3
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(st, {
	agent,
	conn_pid,
	my_snap,
	merged_snap,
	last_ts
}).

%% API

lookup_for(Agent) ->
	gproc:where({n, g, {?MODULE, Agent}}).

start(Agent, Store, Clients, ConnPid) ->
	gen_server:start({via, gproc, {n, g, {?MODULE, Agent}}},
		?MODULE, [Agent, Store, Clients, ConnPid], []).

stop(PidOrAgent) ->
	gen_server:call(server_for(PidOrAgent), stop).

get_snapshot(PidOrAgent) ->
	gen_server:call(server_for(PidOrAgent), get_snapshot).

get_my_rstats(PidOrAgent, Clients, ConnPid) ->
	gen_server:call(server_for(PidOrAgent), {get_my_rstats, Clients, ConnPid}).

%% gen_server callbacks

init([Agent, Store, Clients, ConnPid]) ->
	process_flag(trap_exit, true),

	erlang:monitor(process, ConnPid),

	ClSnap = ouc_rstat:get_client_rstats(),
	Ts = ouc_rstat_snapshot:get(ClSnap, ts),

	Ndxs = create_indexes(Agent, Clients),
	{ok, MySnap} = ouc_rstat_snapshot:new(Store, Ts, Ndxs),

	{ok, MergedSnap} = ouc_rstat_snapshot:merge(ClSnap, MySnap),

    {ok, #st{
    	agent = Agent,
		conn_pid = ConnPid,
    	my_snap = MySnap,
    	merged_snap = MergedSnap,
    	last_ts = Ts
    }}.

handle_call(stop, _From, St) ->
	{stop, normal, ok, St};
handle_call({get_my_rstats, Clients, ConnPid}, _From, St) ->
	LastTs = St#st.last_ts,

	ClSnap = ouc_rstat:get_client_rstats(),
	Ts = ouc_rstat_snapshot:get(ClSnap, ts),

	{Reply, St1} = case Ts > LastTs of
		true ->
			MySnap = St#st.my_snap,
			Agent = St#st.agent,

			Ndxs = create_indexes(Agent, Clients),
			{ok, MySnap1} = ouc_rstat_snapshot:refresh(MySnap, [{ts, Ts}, {indices, Ndxs}]),
			{ok, MergedSnap} = ouc_rstat_snapshot:merge(ClSnap, MySnap),
			R = MergedSnap,
			S = St#st{my_snap=MySnap1, merged_snap=MergedSnap, last_ts=Ts},
			{R, S};
		_ ->
			R = St#st.merged_snap,
			S = St,
			{R, S}
	end,

	ConnPid0 = St1#st.conn_pid,
	St2 = case ConnPid of
		ConnPid0 ->
			St1;
		_ ->
			lager:info("Updating conn pid of rstat watcher to: ~p", [ConnPid]),
			erlang:monitor(process, ConnPid),
			St1#st{conn_pid = ConnPid}
	end,

	{reply, Reply, St2};
handle_call(get_snapshot, _From, St) ->
	Reply = St#st.my_snap,
	{reply, Reply, St};

handle_call(_Request, _From, St) ->
    Reply = ok,
    {reply, Reply, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({'DOWN', _, process, ConnPid, Reason}, #st{conn_pid = ConnPid} = St) ->
	lager:info("Detected death of connection pid: ~p due to ~p. Waiting for reconnection", [ConnPid, Reason]),
	erlang:send_after(60000, self(), {resurrect_timeout, ConnPid}),
	{noreply, St};

handle_info({resurrect_timeout, ConnPid}, #st{conn_pid = ConnPid} = St) ->
	lager:info("Timeout while waiting for reconnection of conn pid: ~p. Stopping.", [ConnPid]),
	{stop, normal, St};

handle_info(_Info, St) ->
    {noreply, St}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%% internal

server_for(Pid) when is_pid(Pid) ->
	Pid;
server_for(Agent) ->
	{via, gproc, {n, g, {?MODULE, Agent}}}.

create_indexes(Agent, Clients) ->
	[[{agent, Agent}, {client, Cl}] || Cl <- Clients].

%% tests

-ifdef(TEST).

-define(M, ?MODULE).

start_stop_test_() ->
	{setup, fun t_setup/0, fun t_teardown/1, fun() ->
		Pid = lookup_for("bea"),
		?assert(is_process_alive(Pid)),

		?M:stop("bea"),
		?assertNot(is_process_alive(Pid))
	end}.

link_test_() ->
	{setup, fun t_setup/0, fun t_teardown/1, fun() ->
		Proxy = cpx_dummy_pid:start_link(),

		WPid = cpx_dummy_pid:do(Proxy, fun() ->
			{ok, Pid} = ?M:start_link("prox", t_store(), []),
			Pid
		end),

		?assert(is_process_alive(WPid)),

		cpx_dummy_pid:stop(Proxy),

		timer:sleep(10),
		?assertNot(is_process_alive(WPid))
	end}.

get_my_rstats_test_() ->
	{setup, fun t_setup/0, fun t_teardown/1, fun() ->
		Snap = ?M:get_my_rstats("bea"),

		?assertEqual(100 / 10, ouc_rstat_snapshot:get_stat_val(Snap, last_30m,
			?PROP_AvE_CALL_DURATION, [{agent, "bea"}, {client, "cl1"}])),
		?assertEqual(250 / 50, ouc_rstat_snapshot:get_stat_val(Snap, last_30m,
			?PROP_AvE_CALL_DURATION, [{client, "cl1"}]))
	end}.

t_setup() ->
	application:start(gproc),

	Store = t_store(),

	{ok, ClSnap} = ouc_rstat_snapshot:new(Store, tm(5), [[{client, "cl1"}],
		[{client, "cl2"}]]),

	meck:new(ouc_rstat),
	meck:expect(ouc_rstat, get_client_rstats, fun() -> ClSnap end),
	?M:start_link("bea", Store, t_clients()).

t_teardown(_) ->
	meck:unload(),
	catch ?M:stop("bea").

t_store() ->
	{ok, Conf} = ouc_rstat_conf:new([{rules, [
			{dynamic, last_30m, {minute, 30}, 5},
			{dynamic, last_15m, {minute, 15}, 5}
		]}]),
	{ok, Store0} = ouc_rstat_store:new(ouc_rstat_store_dict, Conf, []),

	{ok, Store1} = ouc_rstat_store:multi_log(Store0, tm(0),
		[[{agent, "bea"}, {client, "cl1"}]], [
			{?PROP_TE_CALL_DURATION, 100},
			{?PROP_TE_CALL_COUNT, 10}
		]),

	{ok, Store2} = ouc_rstat_store:multi_log(Store1, tm(0),
		[[{client, "cl1"}]], [
			{?PROP_TE_CALL_DURATION, 250},
			{?PROP_TE_CALL_COUNT, 50}
		]),
	Store2.

t_clients() ->
	["cl1", "cl2", "cl3"].

tm(N) ->
	ouc_time:datetime_to_unixts({{2013, 05, 20}, {09, 00, 00}}) + (N*60).

-endif.
