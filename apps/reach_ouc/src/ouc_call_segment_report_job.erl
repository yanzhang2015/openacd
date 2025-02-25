-module(ouc_call_segment_report_job).

-behaviour(gen_leader).

%% API
-export([start_link/1,
        start_link/2,
        tick/0]).

%% gen_leader callbacks
-export([init/1,
         handle_cast/3,
         handle_call/4,
         handle_info/2,
         handle_leader_call/4,
         handle_leader_cast/3,
         handle_DOWN/3,
         elected/3,
         surrendered/3,
         from_leader/3,
         code_change/4,
         terminate/2]).


-define(SERVER, ?MODULE).
-define(INTERVAL, 300). %% seconds interval between script run
-define(PATH, "/scripts/reach-csf/call_segment_facts.js").
-define(CSF_LOG_FILE, "/var/log/sipxpbx/reach-call-segment-facts.log").

-record(state, {cmd, interval}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Nodes) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Nodes) ->
    start_link(Nodes, []).

start_link(Nodes, Seed) when is_list(Nodes), is_atom(Seed) ->
    start_link(Nodes, {seed_node, Seed});

start_link(Nodes, Opts) ->
    gen_leader:start_link(?SERVER, Nodes, Opts, ?MODULE, [], []).

tick() ->
    gen_leader:leader_cast(?MODULE, tick).

%%%===================================================================
%%% gen_leader callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->

    CsfOptions = ouc_config:get_options(reach_ouc, ?MODULE),
    Interval = ouc_config:get_property(interval, CsfOptions, ?INTERVAL),
    Priv = code:priv_dir(reach_ouc),
    Path = Priv ++ ?PATH,
    Cmd = "nice -10 " ++ Path,
    {ok, #state{cmd = Cmd,
                interval = Interval}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called only in the leader process when it is elected. The Synch
%% term will be broadcasted to all the nodes in the cluster.
%%
%% @spec elected(State, Election, undefined) -> {ok, Synch, State}
%% @end
%%--------------------------------------------------------------------
elected(State, _Election, undefined) ->
    Synch = [],
    {ok, Synch, run_jobs(State)};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called only in the leader process when a new candidate joins the
%% cluster. The Synch term will be sent to Node.
%%
%% @spec elected(State, Election, Node) -> {ok, Synch, State}
%% @end
%%--------------------------------------------------------------------
elected(State, _Election, _Node) ->
    {reply, [], State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called in all members of the cluster except the leader. Synch is a
%% term returned by the leader in the elected/3 callback.
%%
%% @spec surrendered(State, Synch, Election) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
surrendered(State, _Synch, _Eelection) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages. Called in the leader.
%%
%% @spec handle_leader_call(Request, From, State, Election) ->
%%                                            {reply, Reply, Broadcast, State} |
%%                                            {reply, Reply, State} |
%%                                            {noreply, State} |
%%                                            {stop, Reason, Reply, State} |
%%                                            {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_leader_call(_Request, _From, State, _Election) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages. Called in the leader.
%%
%% @spec handle_leader_cast(Request, State, Election) ->
%%                                            {ok, Broadcast, State} |
%%                                            {noreply, State} |
%%                                            {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_leader_cast(tick, State, _Election) ->
    {noreply, run_jobs(State)};

handle_leader_cast(_Request, State, _Election) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling messages from leader.
%%
%% @spec from_leader(Request, State, Election) ->
%%                                    {ok, State} |
%%                                    {noreply, State} |
%%                                    {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
from_leader(_Synch, State, _Election) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling nodes going down. Called in the leader only.
%%
%% @spec handle_DOWN(Node, State, Election) ->
%%                                  {ok, State} |
%%                                  {ok, Broadcast, State}
%% @end
%%--------------------------------------------------------------------
handle_DOWN(_Node, State, _Election) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State, Election) ->
%%                                   {reply, Reply, State} |
%%                                   {noreply, State} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State, _Election) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State, Election) ->
%%                                  {noreply, State} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State, _Election) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    {noreply, run_jobs(State)};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_leader when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_leader terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Election, Extra) ->
%%                                          {ok, NewState} |
%%                                          {ok, NewState, NewElection}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Election, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
run_jobs(#state{cmd = Cmd, interval = Interval} = St) ->
    lager:info("Got tick, running report script"),
    spawn(
      fun() ->
        os:cmd(Cmd ++ " >> " ++ ?CSF_LOG_FILE),
        timer:apply_after(Interval * 1000, ouc_call_segment_report_job, tick, [])
      end
    ),
    St.
