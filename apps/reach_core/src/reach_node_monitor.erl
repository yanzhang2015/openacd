-module(reach_node_monitor).
-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([find_previous_node/0, find_next_node/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	monitored_node :: atom() | none,
	ignore_nodedown = false :: boolean()
}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	Node = node(),
	MonitoredNode = case find_previous_node() of
		none ->
			none;
		PrevNode ->
			gproc:add_global_property(reach_node_up, subscribe),
			gproc:add_global_property(reach_node_graceful_shutdown, subscribe),
			erlang:monitor_node(PrevNode, true),
			PrevNode
	end,
	gproc:send({p, g, reach_node_up}, {reach_node_up, Node}),
	lager:info("Monitoring node: ~p", [MonitoredNode]),
	{ok, #state{monitored_node = MonitoredNode}}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.
handle_cast(_Msg, State) ->
	{noreply, State}.
handle_info({nodedown, MonitoredNode}, #state{monitored_node = MonitoredNode, ignore_nodedown = false} = State) ->
	lager:info("Node ~p down", [MonitoredNode]),
	FSNode = get_fs_node(MonitoredNode),

	case catch freeswitch:api(FSNode, show, "calls") of
		{ok, Calls} ->
			CallsList = string:tokens(Calls, "\n"),
			[_ | CallsList1] = CallsList,
			CallsList2 = CallsList1 -- [lists:last(CallsList)],
			lager:info("Number of calls on monitored FS node: ~p", [length(CallsList2)]),

			lists:foreach(fun(C) ->
				CallDetails = string:tokens(C, ","),
				[UUID|_] = CallDetails,
				case freeswitch:api(FSNode, uuid_getvar, UUID ++ " reach_state") of
					{ok, "inqueue"} ->
						{ok, Queue} = freeswitch:api(FSNode, uuid_getvar, UUID ++ " queue"),
						Res = freeswitch_media_manager:recover_call(UUID, FSNode, Queue),
						lager:info("Call recovery result of ~p for call ~p state: ~p, queue: ~p", [Res, UUID, "inqueue", Queue]);
					Els ->
						lager:info("reach_state for ~p: ~p", [UUID, Els])
				end
			end, CallsList2);
		Else ->
			lager:info("Call to remote freeswitch node returned ~p", [Else]),
			ok
	end,
	{noreply, State};
handle_info({nodedown, MonitoredNode}, #state{monitored_node = MonitoredNode} = State) ->
	lager:info("Ignorning nodedown signal for ~p", [MonitoredNode]),
	{noreply, State#state{ignore_nodedown = false}};
handle_info({reach_node_up, MonitoredNode}, #state{monitored_node = MonitoredNode} = State) ->
	lager:info("Re-monitoring node: ~p", [MonitoredNode]),
	erlang:monitor_node(MonitoredNode, true),
	{noreply, State};
handle_info({reach_node_graceful_shutdown, MonitoredNode}, #state{monitored_node = MonitoredNode} = State) ->
	lager:info("Monitored node will shut down gracefully, ignore nodedown signal: ~p", [MonitoredNode]),
	{noreply, State#state{ignore_nodedown = true}};
handle_info(Info, State) ->
	lager:debug("Unknown info: ~p", [Info]),
	{noreply, State}.
terminate(_Reason, _State) ->
	ok.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Helper functions

find_previous_node(Nodes, _) when length(Nodes) < 2 ->
	none;
find_previous_node([Node|Nodes], Node) ->
	lists:last(Nodes);
find_previous_node([Node0,Node|_], Node) ->
	Node0;
find_previous_node([_|Nodes], Node) ->
	find_previous_node(Nodes, Node).

find_previous_node() ->
	case application:get_env(reach_core, nodes) of
		{ok, Nodes} ->
			find_previous_node(Nodes, node());
		_ ->
			none
	end.

find_next_node(Nodes, Node) ->
	find_previous_node(lists:reverse(Nodes), Node).

find_next_node() ->
	case application:get_env(reach_core, nodes) of
		{ok, Nodes} ->
			find_next_node(Nodes, node());
		_ ->
			none
	end.

get_fs_node(Node) ->
	NodeStr = atom_to_list(Node),
	NodeTokens = string:tokens(NodeStr, "@"),
	[_, Domain] = NodeTokens,
	FSNodeStr = "freeswitch@" ++ Domain,
	list_to_atom(FSNodeStr).
