-module(reach_vqueue_manager).

-behaviour(gen_leader).

-include_lib("reach_core/include/call.hrl").
-include_lib("reach_core/include/queue.hrl").


%% API
-export([
	start_link/1,
	start/1,
	add_to_queue/7,
	add_to_queue/6,
	add_to_queue/5,
	add_skills/4,
	add_skills/5,
	set_skills/4,
	remove_skills/4,
	remove_skills/5,
	remove_queue/1,
	add_queue/1,
	get_all/0,
	get_all_info/0,
	get_vqueue_pid/2,
	alive/0,
	find_node/1,
	get_backup/2
]).

%gen_leader support
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
		queues = [],
		chash = undefined
	}).

start(Nodes) ->
	gen_leader:start(?MODULE, Nodes, [{heartbeat, 1}, {vardir, util:run_dir()}], ?MODULE, [], []).

start_link(Nodes) ->
	gen_leader:start_link(?MODULE, Nodes, [{heartbeat, 1}, {vardir, util:run_dir()}], ?MODULE, [], []).

add_to_queue(QName, Node, GenMedia, Callrec, Timestamp, CookPid, Failover) ->
	gen_leader:call(?MODULE, {add, QName, Node, GenMedia, Callrec, Timestamp, CookPid, Failover}).

add_to_queue(QName, Node, GenMedia, Callrec, Timestamp, Failover) ->
	add_to_queue(QName, Node, GenMedia, Callrec, Timestamp, undefined, Failover).

add_to_queue(QName, Node, GenMedia, Callrec, Failover) ->
	add_to_queue(QName, Node, GenMedia, Callrec, undefined, Failover).

set_skills(QName, QPid, CallPid, Skills) ->
	gen_leader:cast(?MODULE, {set_skills, QName, QPid, CallPid, Skills}).

add_skills(QName, QPid, CallPid, Skills) ->
	add_skills(QName, QPid, CallPid, undefined, Skills).

add_skills(QName, QPid, CallPid, Conditions, Skills) ->
	gen_leader:cast(?MODULE, {add_skills, QName, QPid, CallPid, Conditions, Skills}).

remove_skills(QName, QPid, CallPid, Skills) ->
	remove_skills(QName, QPid, CallPid, undefined, Skills).

remove_skills(QName, QPid, CallPid, Conditions, Skills) ->
	gen_leader:cast(?MODULE, {remove_skills, QName, QPid, CallPid, Conditions, Skills}).

remove_queue(QPid) ->
	gen_leader:cast(?MODULE, {remove_queue, QPid}).

add_queue(QPid) ->
	gen_leader:cast(?MODULE, {add_queue, QPid}).

get_all() ->
	gen_leader:leader_call(?MODULE, {get_all}).

get_all_info() ->
	gen_leader:leader_call(?MODULE, {get_all_info}).

%% --------------------------------------------------------------------
-spec(get_vqueue_pid/2 :: (QName :: list(), CallId :: list()) ->
        pid() | undefined).
%% @doc Returns the pid of the vqueue identified by the name
%%  QName, where the call CallId is queued.
%% @end
%% --------------------------------------------------------------------
get_vqueue_pid(QName, CallId) ->
  gen_leader:leader_call(?MODULE, {get_vqueue_pid, QName, CallId}).

get_backup(Node, Index) ->
	gen_leader:leader_call(?MODULE, {get_backup, Node, Index}).

alive() ->
	gen_leader:call(?MODULE, alive).

find_node(IndexAsInt) ->
	gen_leader:call(?MODULE, {get_node, IndexAsInt}).

init([]) ->
	shutdown_manager:subscribe(node(), self()),
	lager:info("New vqueue manager initiated, notifying reach_vqueues..."),
	% gproc:send({p, g, reach_vqueue_manager_change}, {new_vqueue_manager, self()}),
	process_flag(trap_exit, true),
	{ok, #state{}}.

elected(State, Election, Node) ->
	lager:info("elected by ~w", [Node]),
	mnesia:subscribe(system),
	Alive = gen_leader:alive(Election),
	RealAlive = case Node of
		undefined -> Alive;
		_ -> case lists:member(Node, Alive) of
				false -> Alive ++ [Node];
				_ -> Alive
			end
	end,
	lager:info("Alive are ~p", [RealAlive]),
	CHash = chash:fresh(60, node()),
	NewCHash = redistribute(CHash, node(), RealAlive),
	lager:debug("Chash is ~p", [NewCHash]),
	{ok, ok, State#state{chash = NewCHash}}.

surrendered(#state{queues = Queues} = State, _LeaderState, _Election) ->
	lager:info("surrendered", []),
	mnesia:unsubscribe(system),

	% clean out non-local pids
	F = fun(QPid) ->
			node() =:= node(QPid)
	end,
	Locals = lists:filter(F, Queues),
	% and tell the leader about local pids
	gen_leader:leader_cast(?MODULE, {notify_queues, Locals}),
	{ok, State#state{queues = Locals}}.

handle_DOWN(Node, #state{queues = Queues, chash = CHash} = State, Election) ->
	lager:info("Node ~p died/", [Node]),
	% clean out the pids associated w/ the dead node
	F = fun(QPid) ->
			Node =/= node(QPid)
	end,
	Queues2 = lists:filter(F, Queues),
	Alive = gen_leader:alive(Election),
	NewCHash = redistribute(CHash, Node, Alive),
	% lager:info("New CHash is ~p", [NewCHash]),
	{ok, State#state{queues = Queues2, chash = NewCHash}}.

handle_leader_call({get_all}, _From, State, _Election) ->
	{reply, State#state.queues, State};

handle_leader_call({get_all_info}, _From, State, _Election) ->
	Queues = State#state.queues,
	F = fun(Queue, Acc) ->
			case util:is_process_alive(Queue) of
				true -> [{Queue, reach_vqueue:dump(Queue)}|Acc];
				false -> Acc
			end
		end,
	Info = lists:foldl(F, [], Queues),
	{reply, Info, State};

handle_leader_call({get_vqueue_pid, QName, CallId}, _From, State, _Election) ->
  Queues = State#state.queues,
  F = fun(Queue) ->
    case util:is_process_alive(Queue) of
      true ->
        case reach_vqueue:check_queue_for_call(Queue, QName, CallId) of
          true -> false;
          _ -> true
        end;
      false -> true
    end
  end,
  QueuePid = case lists:dropwhile(F, Queues) of
               [] -> undefined;
               [X | _] -> X
             end,
  lager:debug("VQueue pid for name ~p is ~p", [QName, QueuePid]),
  {reply, QueuePid, State};

handle_leader_call({get_backup, Node, Key}, _From, State, _Election) ->
	CHash = State#state.chash,
	lager:debug("CHash: ~p", [CHash]),
	Index = chash:key_of(Key),
	Succs = chash:successors(Index, CHash, 10),
	lager:debug("Succs: ~p ", [Succs]),
	F = fun({_Index, Succ}) ->
			case Succ of
				Node -> false;
				_ -> true
			end
		end,
	BackupNodes = lists:filter(F, Succs),
	BackupNode = case BackupNodes of
		[] -> none;
		[{_, A}|_] -> A
	end,
	{reply, BackupNode, State}.

handle_leader_cast({notify_queues, QPids}, State, _Election) ->
	Queues = State#state.queues,
	NewQs = Queues ++ QPids,
	{noreply, State#state{queues = NewQs}};

handle_leader_cast({notify_down, QPid}, State, _Election) ->
	Queues = State#state.queues,
	NewQs = Queues -- [QPid],
	{noreply, State#state{queues = NewQs}}.

from_leader({update_queues, Queues}, State, _Election) ->
	lager:info("Received queue update from leader, new queue is now ~p", [Queues]),
	{ok, State#state{queues = Queues}};

from_leader(_Msg, State, _Election) ->
	lager:debug("Stub from leader.", []),
	{ok, State}.

handle_call({add, QName, Node, GenMedia, Callrec, Timestamp, CookPid, Failover}, _From, State, Election) ->
	lager:info("Trying to add Call ~p to Queue ~p on node ~p", [Callrec#call.id, QName, Node]),
	Res = get_queue_config(QName, Failover),
	{Reply, State2} = queue(Res, QName, Node, GenMedia, Callrec, Timestamp, CookPid, State, Election),
	{reply, Reply, State2};

handle_call({get_node, IndexAsInt}, From, State, _Election) ->
	lager:info("Received get_node request from ~p", [From]),
	{reply, get_node(IndexAsInt, State#state.chash), State};

handle_call(alive, _From, State, Election) ->
	{reply, gen_leader:alive(Election), State};

handle_call(Request, _From, State, _Election) ->
	{reply, {unknown_call, Request}, State}.

handle_cast({add_skills, QName, QPid, CallPid, Conditions, Skills}, State, Election) ->
	Res = reach_vqueue:get_call(QPid, CallPid),
	lager:info("Vqueue get call result ~p for QPid ~p and CallPid ~p", [Res, QPid, CallPid]),
	lager:info("Qs are ~p", [State#state.queues]),
	{QRes, State2} = case Res of
		none ->
			Callrec = gen_media:get_call(CallPid),
			Skills2 = expand_magic_skills(QName, Callrec, Skills),
			lager:info("Adding skills ~p to call ~p", [Skills2, CallPid]),
			gen_media:add_skills(CallPid, Skills2, Conditions),
			{ok, State};
		{{_Prior, Time}, #queued_call{cook=CookPid, skills=OldSkills} = QCall} ->
			reach_vqueue:remove(QPid, CallPid),
			Skills2 = expand_magic_skills(QName, QCall, Skills),
			lager:info("Callid ~p original skills ~p, skills for addition ~p", [CallPid, OldSkills, Skills2]),
			NewSkills = util:merge_skill_lists(OldSkills, Skills2),
			%%TODO add all back to reach_vqueue_manager
			lager:info("Adding skills ~p to call ~p", [Skills2, CallPid]),
			gen_media:add_skills(CallPid, Skills2, Conditions),
			Callrec = gen_media:get_call(CallPid),
			lager:info("Callrec is ~p", [Callrec]),
			queue(undefined, undefined, node(CallPid), CallPid, Callrec#call{skills=NewSkills}, Time, CookPid, State, Election)
	end,
	case QRes of
		ok -> ok;
		{QN, NewQPid} -> gen_media:set_queue(CallPid, {QN, NewQPid})
	end,
	{noreply, State2};

handle_cast({set_skills, QName, QPid, CallPid, Skills}, State, Election) ->
  Res = reach_vqueue:get_call(QPid, CallPid),
  lager:info("Vqueue get call result ~p for QPid ~p and CallPid ~p", [Res, QPid, CallPid]),
  lager:info("Qs are ~p", [State#state.queues]),
  {QRes, State2} = case Res of
     none ->
       Callrec = gen_media:get_call(CallPid),
       Skills2 = expand_magic_skills(QName, Callrec, Skills),
       lager:info("Setting skills ~p to call ~p", [Skills2, CallPid]),
       gen_media:set_skills(CallPid, Skills2),
       {ok, State};
     {{_Prior, Time}, #queued_call{cook=CookPid, skills=OldSkills} = QCall} ->
       reach_vqueue:remove(QPid, CallPid),
       Skills2 = expand_magic_skills(QName, QCall, Skills),
       lager:info("Callid ~p original skills ~p, setting skills ~p", [CallPid, OldSkills, Skills2]),
       lager:info("Setting skills ~p to call ~p", [Skills2, CallPid]),
       Callrec = gen_media:get_call(CallPid),
       gen_media:set_skills(CallPid, Skills2),
       lager:info("Callrec is ~p", [lager:pr(Callrec, ?MODULE)]),
       queue(undefined, undefined, node(CallPid), CallPid, Callrec#call{skills=Skills2}, Time, CookPid, State, Election)
    end,
  case QRes of
    ok -> ok;
    {QN, NewQPid} -> gen_media:set_queue(CallPid, {QN, NewQPid})
  end,
  {noreply, State2};

handle_cast({remove_skills, QName, QPid, CallPid, Conditions, Skills}, State, Election) ->
	Res = reach_vqueue:get_call(QPid, CallPid),
	lager:info("Vqueue get call result ~p for QPid ~p and CallPid ~p", [Res, QPid, CallPid]),
	{QRes, State2} = case Res of
		none ->
			Callrec = gen_media:get_call(CallPid),
			Skills2 = expand_magic_skills(QName, Callrec, Skills),
			lager:info("Removing skills ~p from call ~p", [Skills2, CallPid]),
			gen_media:remove_skills(CallPid, Skills2, Conditions),
			{ok, State};
		{{_Prior, Time}, #queued_call{cook=CookPid, skills=OldSkills} = QCall} ->
			reach_vqueue:remove(QPid, CallPid),
			lager:info("Removing skills, State: ~p, Condition: ~p", [State, Conditions]),
			Skills2 = expand_magic_skills(QName, QCall, Skills),
			lager:info("Callid ~p original skills ~p, skills for removal ~p", [CallPid, OldSkills, Skills2]),
			NewSkills = lists:subtract(OldSkills, Skills2),
			%%TODO add all back to reach_vqueue_manager
			lager:info("Removing skills ~p from call ~p", [Skills2, CallPid]),
			gen_media:remove_skills(CallPid, Skills2, Conditions),
			Callrec = gen_media:get_call(CallPid),
			lager:info("Callrec is ~p", [Callrec]),
			queue(undefined, undefined, node(CallPid), CallPid, Callrec#call{skills=NewSkills}, Time, CookPid, State, Election)
	end,
	case QRes of
		ok -> ok;
		{QN, NewQPid} -> gen_media:set_queue(CallPid, {QN, NewQPid})
	end,
	{noreply, State2};

handle_cast({remove_queue, QPid}, State, Election) ->
	Qs = State#state.queues,
	NewQs = lists:delete(QPid, Qs),
	case not_leader(Election) of
		true -> gen_leader:leader_cast(?MODULE, {notify_down, QPid});
		false -> ok
	end,
	{noreply, State#state{queues = NewQs}};

handle_cast({add_queue, QPid}, State, _Election) ->
	Qs = State#state.queues,
	NewQs = case lists:member(QPid, Qs) of
				false ->
					lager:info("Queue ~p added to queue list.", [QPid]),
					[QPid|Qs];
				true -> Qs
			end,
	{noreply, State#state{queues = NewQs}};

handle_cast({shutdown, Node}, State, Election) when Node == node() ->
	OwnNode = node(),
	case gen_leader:leader_node(Election) of
		OwnNode ->
			update_member_queues(State#state.queues, Election);
		_ -> ok
	end,
	shutdown_manager:completed(OwnNode, self()),
	{noreply, State};

handle_cast(Msg, State, _Election) ->
	lager:debug("Unhandled cast ~p for ~p", [Msg, ?MODULE]),
	{noreply, State}.

handle_info({shutdown, Node}, State) ->
	gen_leader:cast(?MODULE, {shutdown, Node}),
	{noreply, State};

handle_info(Info, State) ->
	lager:debug("~p got info ~p", [?MODULE, Info]),
	{noreply, State}.

% terminate(shutdown, State) ->
% 	lager:info("Shutting down! Sending shutdown to all local queues."),
% 	Node = node(),
% 	F = fun(Queue) ->
% 			lager:info("Check if local queue ~p", [Queue]),
% 			case node(Queue) of
% 				Node ->
% 					lager:info("Sent shutdown cmd to queue ~p", [Queue]),
% 					reach_vqueue:stop(Queue, shutdown);
% 				_ ->
% 					lager:info("incompatible ~p ~p", [node(Queue), Node]),
% 					ok
% 			end
% 	end,
% 	lager:info("Checking queues ~p", [State#state.queues]),
% 	lists:map(F, State#state.queues),
% 	ok;

terminate(Reason, _State) ->
	lager:notice("~p unusual terminate:  ~p", [?MODULE, Reason]),
	ok.

code_change(_OldVsn, State, _Election, _Extra) ->
	{ok, State}.

get_queue_config(QName, Failover) ->
	case QName of
		undefined -> undefined;
		_ -> case call_queue_config:get_queue(QName) of
				{ok, QRes} -> {QName, QRes};
				_ ->
					lager:info("Queue ~p not found",[QName]),
					case Failover of
						true ->
							lager:info("adding to default queue instead"),
							{ok, DefaultQ} = call_queue_config:get_queue("default_queue"),
							{"default_queue", DefaultQ};
						false ->
							invalid
					end
		end
	end.

get_vqueue(QName, ExpandedSkills, Callrec, Opts, State, Election) ->
	Queues = State#state.queues,
	CHash = State#state.chash,
    Aging = proplists:get_value(aging_factor, Opts, 1),
    lager:info("looking for vqueue with skills ~p, priority ~p, aging ~p",
                    [lists:sort(ExpandedSkills), Callrec#call.priority, Aging]),
    Key = {reach_vqueue, lists:sort(ExpandedSkills), Callrec#call.priority, Aging},
    case gproc:where({n,g, Key}) of
			undefined ->
				lager:info("Vqueue does not exist. Starting Vqueue..."),
				NewCall = case Callrec#call.queue of
							undefined -> Callrec#call{skills=ExpandedSkills, queue = QName};
							_ -> Callrec#call{skills=ExpandedSkills}
				end,
				Index = chash:key_of(Key),
				<<IndexAsInt:160/integer>> = Index,
				SelfNode = node(),
				Node = case gen_leader:leader_node(Election) of
					SelfNode -> get_node(IndexAsInt, CHash);
					Other ->
						lager:info("Sending get_node call to leader"),
						rpc:call(Other, reach_vqueue_manager, find_node, [IndexAsInt])
						% gen_leader:leader_call(?MODULE, {get_node, IndexAsInt})
				end,
				lager:info("Starting vqueue at node ~p", [Node]),
				{ok, StartPid} = reach_vqueue:start_at(Node, NewCall, Opts),
				case not_leader(Election) of
					true -> gen_leader:leader_cast(?MODULE, {notify_queues, [StartPid]});
					false -> ok
				end,
				{StartPid, [StartPid|Queues]};
			ExistPid ->
				lager:info("Vqueue already exists!"),
				{ExistPid, Queues}
	end.

queue(Result, QName, Node, GenMedia, Callrec, Timestamp, CookPid, State, Election) ->
	case Result of
		invalid -> {invalid, State};
		undefined ->
			NewSkills = lists:sort(Callrec#call.skills),
			Skills2 = expand_magic_skills(QName, Callrec, NewSkills),
			ExpandedSkills = lists:usort(Skills2),
			{QPid, NewQ} = get_vqueue("Limbo",ExpandedSkills, Callrec, [], State, Election),
			lager:info("adding Call ~p to Queue ~p", [GenMedia, QPid]),
			reach_vqueue:add(QPid, Callrec#call.priority, Timestamp, Node, GenMedia, CookPid, Callrec),
			gen_media:set_skills(GenMedia, ExpandedSkills),
			{{Callrec#call.queue, QPid}, State#state{queues = NewQ}};
		{QN, Queue} ->
			QGrpSkills = get_qgroup_skills(Queue#call_queue.group),
			QSkills = Queue#call_queue.skills,
			NewSkills = lists:umerge3(lists:usort(QSkills), lists:usort(Callrec#call.skills), lists:usort(QGrpSkills)),
			lager:info("Callrec is ~p, New Skills are ~p", [Callrec, NewSkills]),
			Skills2 = expand_magic_skills(QName, Callrec, NewSkills),
			ExpandedSkills = lists:usort(Skills2),
			Callrec2 = set_queue_priority(Callrec#call{skills = ExpandedSkills}, Queue),
			lager:info("Start cook for call"),
			%%TODO move cook call outside queue/7
			{ok, NewCall, Cook} = case CookPid of
				undefined -> cook:start_cook_for_call(QN, GenMedia, Callrec2#call{skills = ExpandedSkills}, Queue#call_queue.recipe, util:now_ms());
				_ -> {ok, Callrec2#call{skills = ExpandedSkills}, CookPid}
			end,
			Opts = queue_to_opts(Queue#call_queue{skills = NewCall#call.skills}),
			{QPid, NewQs} = get_vqueue(QN, NewCall#call.skills, NewCall, Opts, State, Election),
			cook:set_queue(Cook, QPid),
			lager:info("adding Call ~p to Queue ~p", [GenMedia, QPid]),
			reach_vqueue:add(QPid, NewCall#call.priority, Timestamp, Node, GenMedia, Cook, NewCall),
			gen_media:set_skills(GenMedia, NewCall#call.skills),
			{{QN, QPid}, State#state{queues = NewQs}}
	end.

set_queue_priority(Callrec, Queue) ->
	NewPrio = case Callrec#call.priority of
		undefined -> Queue#call_queue.weight * -1;
		Prio -> Prio
	end,
	Callrec#call{priority = NewPrio}.

get_qgroup_skills(QGroup) ->
	Res = call_queue_config:get_queue_group(QGroup),
	case Res of
		{ok, QGroupRec} ->
					QGroupRec#queue_group.skills;
		_ -> lager:error("Error encountered finding queue group ~p", [QGroup]),
			 []
	end.

queue_to_opts(Queuerec) ->
	[
		{recipe, Queuerec#call_queue.recipe},
		{weight, Queuerec#call_queue.weight},
		{skills, Queuerec#call_queue.skills},
		{group, Queuerec#call_queue.group},
		{wrapup_enabled, Queuerec#call_queue.wrapup_enabled},
		{wrapup_timer, Queuerec#call_queue.wrapup_timer},
		{auto_wrapup, Queuerec#call_queue.auto_wrapup},
		{aging_factor, Queuerec#call_queue.aging_factor}
	].

%% @private
-spec(expand_magic_skills/3 :: (Name :: list(), Call :: #queued_call{} | #call{}, Skills :: [atom()]) -> [atom()]).
expand_magic_skills(Name, QCall, Skills) when is_record(QCall, queued_call) ->
	%% TODO check why gen_media call is still necessary
	Call = gen_media:get_call(QCall#queued_call.media),
	expand_magic_skills(Name, Call, Skills);
expand_magic_skills(Name, Call, Skills) ->
	lager:info("inside expansion and Call is ~p", [Call]),
	Unfiltered = [case Skill of
		'_node' -> {Skill, node(Call#call.source)};
		'_queue' -> {Skill, Name};
		'_brand' ->
			case Call#call.client of
			Client when is_record(Client, client) -> {Skill, Client#client.label};
			_ -> {Skill, undefined}
		end;
		_ -> Skill
	end	|| Skill <- Skills],
	[X || X <- Unfiltered, X =/= []].

redistribute(CHash, ReplaceNode, Alive) ->
	{NumPartitions, Nodes} = CHash,
	NumAlive = length(Alive),
	F = fun({Index, Node}, Counter) ->
		% lager:info("Node is ~p, ReplaceNode is ~p", [Node, ReplaceNode]),
		case Node of
			ReplaceNode -> {{Index, lists:nth((Counter rem NumAlive) + 1, Alive)}, Counter + 1};
			_Else -> {{Index, Node}, Counter}
		end
	end,
	{NewNodes, NodesChanged} = lists:mapfoldl(F, 0, Nodes),
	lager:info("Number of nodes changed : ~p", [NodesChanged - 1]),
	{NumPartitions, NewNodes}.

get_node(IndexAsInt, CHash) ->
	NodeIndex = chash:next_index(IndexAsInt, CHash),
	chash:lookup(NodeIndex, CHash).

not_leader(Election) ->
	Leader = gen_leader:leader_node(Election),
	Leader =/= node().

update_member_queues(Queues, Election) ->
	Alive = gen_leader:alive(Election),
	Workers = Alive -- [node()],
	lager:info("Sending subscription broadcast to members ~p", [Workers]),
	gen_leader:broadcast({from_leader, {update_queues, Queues}}, Workers, Election).
