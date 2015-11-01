-module(shutdown_manager).

-behaviour(gen_leader).


%% API
-export([
	start_link/1,
	start/1,
	subscribe/2,
	unsubscribe/2,
	shutdown/1,
	completed/2,
	update_state/1,
	dump/0
]).

-define(SHUTDOWN_TIMEOUT, 15000).

-export([init/1,
		elected/3,
		surrendered/3,
		handle_DOWN/3,
		handle_leader_call/4,
		handle_leader_cast/3,
		from_leader/3,
		handle_call/4,
		handle_cast/3,
		handle_info/2,
		terminate/2,
		code_change/4]).

-record(state, {
		subs = [], %% [{'reach@one.ezuce.ph', [<0.234.0>, <0.236.0>]}]
		current_node = undefined,
		nodes_queued = [],
		pending = 0,
		timer = undefined
	}).

%% API

start(Nodes) ->
	gen_leader:start(?MODULE, Nodes, [{heartbeat, 1}, {vardir, util:run_dir()}], ?MODULE, [], []).

start_link(Nodes) ->
	gen_leader:start_link(?MODULE, Nodes, [{heartbeat, 1}, {vardir, util:run_dir()}], ?MODULE, [], []).

subscribe(Node, Pid) ->
	gen_leader:leader_cast(?MODULE, {subscribe, Node, Pid}).

unsubscribe(Node, Pid) ->
	gen_leader:leader_cast(?MODULE, {unsubscribe, Node, Pid}).

shutdown(Node) ->
	gen_leader:leader_cast(?MODULE, {shutdown, Node}).

completed(Node, Pid) ->
	gen_leader:leader_cast(?MODULE, {completed, Node, Pid}).

update_state(State) ->
	gen_leader:cast(?MODULE, {update_state, State}).

dump() ->
	gen_leader:call(?MODULE, dump).

%% GEN_LEADER FUNCTIONS

init([]) ->
	lager:info("Shutdown manager initiated"),
	process_flag(trap_exit, true),
	{ok, #state{}}.

elected(State, _Election, undefined) ->
	lager:info("elected by undefined"),
	{ok, ok, State};

elected(State, Election, Node) ->
	lager:info("elected by ~w", [Node]),
	update_members(Node, State, Election),
	{ok, ok, State}.

surrendered(State, _LeaderState, Election) ->
	lager:info("surrendered", []),
	Node = node(),
	case gen_leader:leader_node(Election) of
		Node ->
			gen_leader:leader_cast(?MODULE, {update_state, State});
		Else ->
			lager:info("Not leader, leader is ~p", [Else]),
			ok
	end,
	{ok, State}.

handle_DOWN(Node, State, _Election) ->
	lager:info("Node ~p died/", [Node]),
	{ok, State}.

handle_leader_call(Msg, _From, State, _Election) ->
	lager:debug("Unhandled cast ~p for ~p", [Msg, ?MODULE]),
	{reply, {unknown_msg, Msg}, State}.

handle_leader_cast({subscribe, Node, Pid}, State, Election) ->
	Subs = State#state.subs,
	NewSubs = case proplists:get_value(Node, Subs) of
		undefined ->
			lager:info("Process ~p now subscribed to leader for shutdown of Node ~p", [Pid, Node]),
			[{Node, [Pid]}|Subs];
		NodeSubs ->
			lager:info("Process ~p now subscribed to shutdown of Node ~p", [Pid, Node]),
			NewNodeSubs = {Node, [Pid|NodeSubs]},
			lists:keyreplace(Node, 1, Subs, NewNodeSubs)
	end,
	NewState = State#state{subs = NewSubs},
	update_members(NewState, Election),
	{noreply, NewState};

handle_leader_cast({unsubscribe, Node, Pid}, State, Election) ->
	Subs = State#state.subs,
	NewSubs = case proplists:get_value(Node, Subs) of
		undefined -> Subs;
		NodeSubs -> NewNodeSubs = {Node, lists:delete(Pid, NodeSubs)},
					lists:keyreplace(Node, 1, Subs, NewNodeSubs)
	end,
	NewState = State#state{subs = NewSubs},
	update_members(NewState, Election),
	{noreply, NewState};

handle_leader_cast({shutdown, Node}, State, Election) ->
	NewState =
		case State#state.current_node of
			undefined ->
				gproc:send({p, g, reach_node_graceful_shutdown}, {reach_node_graceful_shutdown, Node}),
				lager:info("Sending shutdown msg to subscribed ~p", [State#state.subs]),
				Pending = send_shutdown_msg(Node, State#state.subs),
				lager:info("Waiting for ~p processes to finish", [Pending]),
				drop_node(Node, Election),
				case Pending of
					0 ->
						NewSubs = remove_node(Node, State#state.subs),
						State2 = State#state{current_node = undefined, subs = NewSubs},
						update_members(State2, Election),
						rpc:call(Node, erlang, halt, []),
						State2;
					Val ->
						{ok, TRef} = timer:send_after(?SHUTDOWN_TIMEOUT, {shutdown_timeout, Node}),
						State#state{current_node = Node, pending = Val, timer = TRef}
				end;
			Node -> lager:info("Already shutting down self!"),
					State;
			Else -> lager:info("Still shutting down ~p, wait a while...", [Else]),
					Queued = State#state.nodes_queued ++ [Node],
					State#state{nodes_queued = Queued}
		end,
	update_members(NewState, Election),
	{noreply, NewState};

handle_leader_cast({completed, Node, Pid}, #state{current_node = CurrentNode} = State,
							 Election) when Node == CurrentNode ->
	lager:info("received completed from process ~p", [Pid]),
	NewState =
	case State#state.pending of
		1 -> case State#state.nodes_queued of
				[] ->
					NewSubs = remove_node(Node, State#state.subs),
					State2 = State#state{current_node = undefined, nodes_queued = [],
					pending = 0, timer = undefined, subs = NewSubs},
					update_members(State2, Election),
					timer:cancel(State#state.timer),
					rpc:call(Node, erlang, halt, []),
					State2;
				[NewNode|Others] ->
									timer:cancel(State#state.timer),
									Pending = send_shutdown_msg(NewNode, State#state.subs),
									NewSubs = remove_node(Node, State#state.subs),
									State2 = State#state{current_node = NewNode, nodes_queued = Others,
									pending = Pending, timer = undefined, subs = NewSubs},
									update_members(State2, Election),
									rpc:call(Node, erlang, halt, []),
									State2
			 end;
		Remaining ->
			State#state{pending =  Remaining - 1}
	end,
	update_members(NewState, Election),
	{noreply, NewState}.

from_leader({subscribe, Node, Pid}, State, _Election) ->
	Subs = State#state.subs,
	NewSubs = case proplists:get_value(Node, Subs) of
		undefined ->
			lager:info("Received msg from leader of subscription by ~p for shutdown of Node ~p", [Pid, Node]),
			[{Node, [Pid]}|Subs];
		NodeSubs ->
			lager:info("Received msg from leader of subscription by ~p for shutdown of Node ~p", [Pid, Node]),
			NewNodeSubs = {Node, [Pid|NodeSubs]},
			lists:keyreplace(Node, 1, Subs, NewNodeSubs)
	end,
	{noreply, State#state{subs = NewSubs}};

from_leader({unsubscribe, Node, Pid}, State, _Election) ->
	Subs = State#state.subs,
	NewSubs = case proplists:get_value(Node, Subs) of
		undefined -> Subs;
		NodeSubs -> NewNodeSubs = {Node, lists:delete(Pid, NodeSubs)},
					lists:keyreplace(Node, 1, Subs, NewNodeSubs)
	end,
	{noreply, State#state{subs = NewSubs}};

from_leader({drop_node, Node}, State, _Election) ->
	lager:info("Received drop_node ~p from leader, State is now ~p", [Node, State]),
	Subs = State#state.subs,
	NewSubs = proplists:delete(Node, Subs),
	{noreply, State#state{subs = NewSubs}};

from_leader({update, NewState}, State, _Election) ->
	lager:info("State update from leader, State is now ~p", [State]),
	{noreply, NewState};

from_leader(_Msg, State, _Election) ->
	lager:debug("Stub from leader.", []),
	{ok, State}.

handle_call(dump, _From, State, Election) ->
	{reply, {state, State, election, Election}, State};

handle_call(Request, _From, State, _Election) ->
	{reply, {unknown_call, Request}, State}.

handle_cast({update_state, State}, _OldState, _Election) ->
	{noreply, State};

handle_cast({shutdown_timeout, Node}, State, Election) ->
	lager:info("Reached shutdown TIMEOUT... shutting down IMMEDIATELY!"),
	NewState = case State#state.nodes_queued of
		[] ->
			timer:cancel(State#state.timer),
			NewSubs = remove_node(Node, State#state.subs),
			State2 = State#state{current_node = undefined, nodes_queued = [],
			pending = 0, timer = undefined, subs = NewSubs},
			update_members(State2, Election),
			rpc:call(Node, erlang, halt, []),
			State2;
		% [OnlyNode] ->
		%			timer:cancel(State#state.timer),
			%		Pending = send_shutdown_msg(OnlyNode, State#state.subs),
			%		rpc:call(Node, erlang, halt, []),
			%		State#state{current_node = OnlyNode, nodes_queued = [], pending = Pending, timer = undefined};
		[NewNode|Others] ->
							timer:cancel(State#state.timer),
							NewSubs = remove_node(Node, State#state.subs),
							Pending = send_shutdown_msg(NewNode, State#state.subs),
							State2 = State#state{current_node = NewNode, nodes_queued = Others,
							pending = Pending, timer = undefined, subs = NewSubs},
							update_members(State2, Election),
							rpc:call(Node, erlang, halt, []),
							State2
	end,
	{noreply, NewState};

handle_cast(Msg, State, _Election) ->
	lager:debug("Unhandled cast ~p for ~p", [Msg, ?MODULE]),
	{noreply, State}.

handle_info({shutdown_timeout, Node}, #state{current_node = Node} = State) ->
	gen_leader:cast(?MODULE, {shutdown_timeout, Node}),
	{noreply, State};

handle_info(Info, State) ->
	lager:debug("~p got info ~p", [?MODULE, Info]),
	{noreply, State}.

terminate(Reason, _State) ->
	lager:notice("~p unusual terminate:  ~p", [?MODULE, Reason]),
	ok.

code_change(_OldVsn, State, _Election, _Extra) ->
	{ok, State}.

%% INTERNAL

send_shutdown_msg(Node, Subs) ->
	lager:info("Sending SHUTDOWN signal for Node ~p to Subs ~p", [Node, Subs]),
	case proplists:get_value(Node, Subs) of
		undefined -> 0;
		Pids -> [Pid ! {shutdown, Node} || Pid <- Pids],
				length(Pids)
	end.

drop_node(Node, Election) ->
	Alive = gen_leader:alive(Election),
	Workers = Alive -- [node()],
	lager:info("Sending drop_node broadcast to workers ~p", [Workers]),
	gen_leader:broadcast({from_leader, {drop_node, Node}}, Workers, Election).

remove_node(Node, Subs) ->
	lists:keydelete(Node, 1, Subs).

update_members(State, Election) ->
	update_members(undefined, State, Election).

update_members(undefined, State, Election) ->
	Alive = gen_leader:alive(Election),
	Workers = Alive -- [node()],
	lager:info("Sending subscription broadcast to members ~p", [Workers]),
	gen_leader:broadcast({from_leader, {update, State}}, Workers, Election);

update_members(Node, State, Election) ->
	Alive = gen_leader:alive(Election),
	Temp = Alive -- [node()],
	Workers = [Node|Temp],
	lager:info("Sending subscription broadcast to members ~p", [Workers]),
	gen_leader:broadcast({from_leader, {update, State}}, Workers, Election).
