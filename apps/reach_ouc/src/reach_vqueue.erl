-module(reach_vqueue).

-behaviour(gen_server).

-include_lib("reach_core/include/call.hrl").
-include_lib("reach_core/include/queue.hrl").
-include_lib("reach_core/include/agent.hrl").

-export([
	start/2,
	start/1,
	start_link/2,
	start_link/1,
	start_at/3,
	start_at/2,
	add/7,
	add/6,
	add/5,
	add/4,
	add/3,
	add/2,
	add_at/3,
	handshake/4,
	request_call/3,
	get_agents/1,
	remove_agent/3,
	remove_eligible/3,
	remove_call/2,
	remove_cook/2,
	assign_call/2,
	get_call/2,
	get_name/1,
	get_skills/2,
	check_queue_for_call/3,
	requeue/4,
	set_priority/3,
	remove/2,
	bgremove/2,
	to_list/1,
	call_count/1,
	call_count_by_client/2,
	dump/1,
	stop/1,
	stop/2
]).

-define(QUEUE_TIMEOUT, 3600000).
-define(REQUEUE_TIMEOUT, 5000).

-type(call_queue() :: gb_tree()).

-record(state, {
	name = undefined :: undefined | string(),
	started = undefined,
	queue = gb_trees:empty() :: call_queue(),
	limbo_calls = [],
	recipe = ?DEFAULT_RECIPE :: recipe(),
	call_skills = [english, '_node'] :: [atom()],
	flag = undefined,
	priority = 1 :: integer(),
	aging_factor = 1 :: float(),
	eligible_agents = [],
	free_agents = [],
	end_timer = undefined,
	incarnation = 0,
	notified = 0,
	subscribed = []
}).

%gen_server support
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type(opt_name() :: 'weight' | 'recipe' | 'skills' | 'group').
-type(start_opt() :: {opt_name(), any()} | 'default_queue').
-type(opts() :: [start_opt()]).

%% @doc Start a queue named `Name' with no link to the current process.
%% Passing in `default_queue' as a flag means the queue cannot exit when
%% empty.
-spec(start/2 :: (Callrec :: #call{}, Opts :: opts()) -> {ok, pid()}).
start(Callrec, Opts) -> % Start linked queue custom default recipe and weight
	gen_server:start(?MODULE, [Callrec, Opts], []).

start(State) ->
	gen_server:start(?MODULE, State, []).

%% @doc Start a queue names `Name' with a link to the current process.
%% @see start/3
-spec(start_link/2 :: (Callrec :: #call{}, Opts :: opts()) -> {ok, pid()}).
start_link( Callrec, Opts) -> % Start linked queue with custom recipe and weight
	gen_server:start_link(?MODULE, [Callrec, Opts], []).

start_link(State) ->
	gen_server:start_link(?MODULE, State, []).

start_at(Node, Callrec, Opts) ->
	rpc:call(Node, reach_vqueue, start, [Callrec, Opts]).

start_at(Node, State) ->
	rpc:call(Node, reach_vqueue, start, [State]).

add(Pid, Priority, Timestamp, Node, Mediapid, CookPid, Callrec) ->
	gen_server:cast(Pid, {add, Priority, Timestamp, Node, Mediapid, CookPid, Callrec}).

add(Pid, Priority, Timestamp, Mediapid, CookPid, Callrec) ->
	add(Pid, Priority, Timestamp, node(Mediapid), Mediapid, CookPid, Callrec).

add(Pid, Priority, Timestamp, Mediapid, Callrec) ->
	add(Pid, Priority, Timestamp, Mediapid, Callrec#call.cook, Callrec).

%% @doc Add to the queue at `pid()' `Pid' a new call with `pos_integer()'
%% `Priority', `pid()' Mediapid, and `#call{}' Callrec.
-spec(add/4 :: (Pid :: pid(), Priority :: call_priority(), Mediapid :: pid(), Callrec :: #call{}) -> ok).
add(Pid, Priority, Mediapid, Callrec) when is_pid(Pid), is_pid(Mediapid), Priority >= 0, is_record(Callrec, call) ->
	add(Pid, Priority, undefined, Mediapid, Callrec);

add(Pid, Priority, Timestamp, Mediapid) when is_pid(Pid), is_pid(Mediapid), Priority >= 0, Timestamp > 0 ->
	Callrec = gen_media:get_call(Mediapid),
	add(Pid, Priority, Timestamp, Mediapid, Callrec).

%% @doc Add to the queue at `pid()' `Pid' a call with default values.
%% Second paramter can be either `pos_integer()' `Priority' or `pid()'
%% `Mediapid'.  If the second parameter is `Priority', the 3rd parameter
%% must be the `pid()' `Mediapid'.  A new call is queued using the given
%% `Priority', `Mediapid', and result of
%% `gen_server:call(Mediapid, get_call)'.
%%
%% If the second parameter is `Mediapid', the 3rd parameter must be
%% `#call{}' `Callrec'.  A new call is queued using the default priority
%% of 1.
%% @see add/4
-spec(add/3 :: (Pid :: pid(), Priority :: call_priority(), Mediapid :: pid()) -> ok;
	(Pid :: pid(), Mediapid :: pid(), Calldata :: #call{}) -> ok).
add(Pid, Mediapid, Callrec) when is_pid(Pid), is_pid(Mediapid), is_record(Callrec, call) ->
	add(Pid, Callrec#call.priority, Mediapid, Callrec);
add(Pid, Priority, Mediapid) when is_pid(Pid), is_pid(Mediapid), Priority >= 0 ->
	Callrec = gen_media:get_call(Mediapid),
	add(Pid, Priority, Mediapid, Callrec).

%% @doc Add to queue at `pid()' `Pid' a call taken from `pid()' `Mediapid'.
%% A call record is gather from the media server at `Mediapid', and then
%% added to the queue with a priority of 1.
%% @see add/3
%% @see add/4
-spec(add/2 :: (Pid :: pid(), Mediapid :: pid()) -> ok).
add(Pid, Mediapid) when is_pid(Pid), is_pid(Mediapid) ->
	add(Pid, 1, Mediapid).

add_at(Pid, Key, Mediapid) ->
	Callrec = gen_media:get_call(Mediapid),
	gen_server:cast(Pid, {add_at, Key, Mediapid, Callrec}).

handshake(Pid, AgentPid, AgentLogin, AgentSkills) ->
	gen_server:cast(Pid, {handshake, AgentPid, AgentLogin, AgentSkills}).

request_call(Pid, AgentPid, AgentLogin) ->
	gen_server:cast(Pid, {request_call, AgentPid, AgentLogin}).

remove_call(Pid, Data) ->
	gen_server:cast(Pid, {remove_call, Data}).

remove_cook(Pid, CookPid) ->
	gen_server:cast(Pid, {remove_cook, CookPid}).

assign_call(Pid, AgentPid) ->
	gen_server:cast(Pid, {assign_call, AgentPid}).

get_call(Pid, CallPid) ->
	gen_server:call(Pid, {get_call, CallPid}).

get_agents(Pid) ->
	gen_server:call(Pid, get_agents).

get_skills(Pid, Call) ->
	gen_server:call(Pid, {get_skills, Call}).

get_name(Pid) ->
	gen_server:call(Pid, get_name).

remove_agent(Pid, AgentPid, AgentLogin) ->
	gen_server:cast(Pid, {remove_agent, AgentPid, AgentLogin}).

remove_eligible(Pid, AgentPid, AgentLogin) ->
	gen_server:cast(Pid, {remove_eligible, AgentPid, AgentLogin}).

requeue(Pid, Key, CallPid, Callrec) ->
	gen_server:cast(Pid, {requeue, Key, CallPid, Callrec}).

set_priority(Pid, Mediaid, Priority) when Priority >= 0 ->
	gen_server:call(Pid, {set_priority, Mediaid, Priority});
set_priority(_Pid, _Mediapid, _Priority) ->
	none.

remove(Pid, Callpid) when is_pid(Pid), is_pid(Callpid) ->
	gen_server:call(Pid, {remove, Callpid});
remove(Pid, Callid) when is_pid(Pid) ->
	case gen_server:call(Pid, {get_call, Callid}) of
		{_Key, #queued_call{media=Mediapid}} ->
			remove(Pid, Mediapid);
		none ->
			none
	end.

bgremove(Pid, Callpid) when is_pid(Pid), is_pid(Callpid) ->
	gen_server:cast(Pid, {remove_call, Callpid});
bgremove(Pid, Callid) when is_pid(Pid) ->
	case gen_server:call(Pid, {get_call, Callid}) of
		{_Key, #queued_call{media = Mediapid}} ->
			bgremove(Pid, Mediapid);
		none ->
			ok
	end.

to_list(Pid) ->
	gen_server:call(Pid, to_list).

call_count(Pid) ->
	gen_server:call(Pid, call_count).

call_count_by_client(Pid, Client) ->
	gen_server:call(Pid, {call_count_by_client, Client}).

dump(Pid) ->
	gen_server:call(Pid, dump).

%% --------------------------------------------------------------------
-spec(check_queue_for_call/3 :: (Pid :: pid(), QName :: list(),
    CallId :: list()) -> pid() | undefined).
%% @doc Returns true if the call identified by CallId is queued
%%  on the vqueue with the name QName and false otherwise.
%% @end
%% --------------------------------------------------------------------
check_queue_for_call(Pid, QName, CallId) ->
  gen_server:call(Pid, {check_queue, QName, CallId}).

stop(Pid) ->
	stop(Pid, normal).

stop(Pid, Reason) ->
	gen_server:cast(Pid, {stop, Reason}).
%=====
% gen_server callbacks
%=====

%% @private
init([Callrec, Opts]) ->
	lager:info("subscribing to shutdown manager with pid ~p for node ~p", [self(), node()]),
	shutdown_manager:subscribe(node(), self()),
	lager:info("Starting at ~p with opts : ~p", [node(), Opts]),
	process_flag(trap_exit, true),
	State = #state{
		recipe = proplists:get_value(recipe, Opts, ?DEFAULT_RECIPE),
		call_skills = lists:sort(Callrec#call.skills),
		name = Callrec#call.queue,
		priority = Callrec#call.priority,
		aging_factor = proplists:get_value(aging_factor, Opts, 1),
		started = ouc_time:now_micro(),
		flag = case proplists:get_value(default_queue, Opts) of
			true -> default_queue;
			_ -> undefined
		end
	},
	lager:info("registering vqueue with skills ~p, priority ~p, aging ~p",
	[State#state.call_skills, State#state.priority, State#state.aging_factor]),
	gproc:reg({n, g, {reach_vqueue, State#state.call_skills, State#state.priority, State#state.aging_factor}}),
	Key = {n, g, {reach_vqueue, State#state.call_skills, State#state.priority, State#state.aging_factor, State#state.incarnation}},
	gproc:reg(Key),
	gproc:send({p, g, {reach_vqueue_registered, Key}}, {gproc, registered, Key}),
	% gproc:add_global_property(reach_vqueue_manager_change, subscribe),
	% init_gproc_prop(State),
	% set_cpx_mon(State, self()),
	{ok, State, 0};

init(State) ->
	lager:info("subscribing to shutdown manager with pid ~p for node ~p", [self(), node()]),
	shutdown_manager:subscribe(node(), self()),
	NewState = State#state{
		started = ouc_time:now_micro(),
		eligible_agents = [],
		free_agents = [],
		end_timer = undefined
	},
	lager:info("registering vqueue with State ~p",
	[NewState]),
	gproc:reg({n, g, {reach_vqueue, NewState#state.call_skills, NewState#state.priority, NewState#state.aging_factor}}),
	gproc:reg({n, g, {reach_vqueue, NewState#state.call_skills, NewState#state.priority, NewState#state.aging_factor, NewState#state.incarnation}}),
	reach_vqueue_manager:add_queue(self()),
	% gproc:add_local_property(reach_vqueue_manager_change, subscribe),
	% init_gproc_prop(State),
	% set_cpx_mon(State, self()),
	{ok, NewState, 0}.

%% =====
%% handle_call
%% =====

handle_call({get_call, Callpid}, _From, State) when is_pid(Callpid) ->
	{reply, find_by_pid(Callpid, State#state.queue), State};
handle_call({get_call, Callid}, _From, State) ->
	{reply, find_key(Callid, State#state.queue), State};

handle_call({set_priority, Id, Priority}, _From, State) when is_pid(Id), Priority >= 0 ->
	case find_by_pid(Id, State#state.queue) of
		none ->
			{reply, none, State};
		{{Oldpri, Time}, Value} ->
			State2 = State#state{queue=try_delete({Oldpri, Time}, State#state.queue)},
			State3 = State2#state{queue=gb_trees:insert({Priority, Time}, Value, State2#state.queue)},
			{reply, ok, State3}
	end;

handle_call({set_priority, Id, Priority}, _From, State) when Priority >= 0 ->
	case find_key(Id, State#state.queue) of
		none ->
			{reply, none, State};
		{{Oldpri, Time}, Value} ->
			State2 = State#state{queue=try_delete({Oldpri, Time}, State#state.queue)},
			State3 = State2#state{queue=gb_trees:insert({Priority, Time}, Value, State2#state.queue)},
			{reply, ok, State3}
	end;

handle_call(call_count, _From, State) ->
	{reply, gb_trees:size(State#state.queue), State};

handle_call({call_count_by_client, Client}, _From, State) ->
	% TODO - if this is too inefficent, micah can make cpx_monitor do it
	Count = length(lists:filter(
		fun(Qcall) ->
				#call{client = Client2} = gen_media:get_call(Qcall#queued_call.media), Client#client.id == Client2#client.id
		end, gb_trees:values(State#state.queue))),
	{reply, Count, State};

handle_call(to_list, _From, State) ->
	{reply, gb_trees:values(State#state.queue), State};

handle_call(get_agents, _From, State) ->
	{reply, State#state.free_agents, State};

handle_call(get_name, _From, State) ->
	{reply, State#state.name, State};

handle_call({get_skills, Call}, _From, State) ->
	Skills = expand_magic_skills(Call#call.queue, Call, State#state.call_skills),
	{reply, Skills, State};

handle_call({remove, Callpid}, _From, State) ->
	lager:info("Trying to remove call ~p from ~p", [Callpid, undefined_name]),
	case find_by_pid(Callpid, State#state.queue) of
		none ->
			lager:info("Did not find call ~w in ~p", [Callpid, undefined_name]),
			{reply, none, State};
		{Key, Qcall} ->
			NewQ = try_delete(Key, State#state.queue),
			State2 = State#state{queue=NewQ},
			% lists:foreach(fun(D) -> exit(D, kill) end, Qcall#queued_call.dispatchers),
			set_cpx_mon(State2),
			lager:info("Removed call ~p from queue ~p", [Qcall#queued_call.id, undefined_name]),
			EndTimer = get_expire_timer(NewQ, State),
			{reply, ok, State2#state{end_timer = EndTimer}}
	end;

handle_call(dump, _From, State) ->
	{reply, State, State};

handle_call({check_queue, QName, CallId}, _From, State) ->
  CallList = gb_trees:values(State#state.queue),
  Reply = if State#state.name =:= QName ->
            case lists:keyfind(CallId, 4, CallList) of
              false -> false;
              _ -> true
            end
           ; true  -> false
          end,
  {reply, Reply, State};

handle_call(Request, _From, State) ->
	{reply, {unknown_call, Request}, State}.

%% =====
%% handle_cast
%% =====

%% @private

handle_cast({add, Priority, Timestamp, Node, CallPid, CookPid, Callrec}, State) when is_pid(CallPid) ->
	cancel_timer(State),
	lager:info("Trying to add call ~p to self ~p", [CallPid, self()]),
	Subscribed = subscribe(Node, State#state.subscribed),
	Time = case Timestamp of
			undefined -> ouc_time:now_micro();
			Value -> Value
	end,
	Key = {Priority, Time},
	gen_media:set_queue_key(CallPid, Key),
	case CookPid of
		undefined -> ok;
		_ -> cook:set_queue(CookPid, self())
	end,
	NewState = queue_call(Node, CookPid, Callrec, Key, State),
	Agents2 = case State#state.free_agents of
		[] ->
			lager:info("No free agents"),
			[];
		Agents ->
			% notify(Agent),
			RealAgents = lists:reverse(Agents),
			[{TopLogin, TopAgent}|RemAgents] = RealAgents,
			lager:info("free agents in queue, notifying top agent ~p ~p", [TopLogin, TopAgent]),
			agent:notify_call(TopAgent),
			lists:reverse(RemAgents)
	end,
	{noreply, NewState#state{end_timer = undefined, free_agents = Agents2, subscribed = Subscribed}};

handle_cast({add_at, Key, Mediapid, Mediarec}, #state{name = QName} = State) ->
	cancel_timer(State),
	lager:info("adding call ~p to ~p on node ~p at position ~p", [Mediarec#call.id, QName, node(Mediapid), Key]),
	% cook is started on the same node Mediapid is on
	{ok, Cookpid} = cook:start_at(node(Mediapid), Mediapid, State#state.recipe, QName, self(), Key),
	NewState = queue_call(node(Mediapid), Cookpid, Mediarec, Key, State),
	{noreply, NewState};

handle_cast({handshake, AgentPid, AgentLogin, AgentSkills},
			#state{call_skills = QSkills, eligible_agents = Eligible} = State) ->
	{Reply, NewEligible} = case (QSkills -- AgentSkills) of
		[] -> {accept, [{AgentLogin, AgentPid}|Eligible]};
		_ -> {reject, Eligible}
	end,
	lager:info("Replying to Agent ~p ~p with handshake ~p", [AgentLogin, AgentPid, Reply]),
	lager:info("Eligible agent list is now ~p", [NewEligible]),
	agent:reply_handshake(AgentPid, self(), Reply),
	{noreply, State#state{eligible_agents = NewEligible}};

handle_cast({request_call, AgentPid, AgentLogin}, #state{queue = Queue, free_agents = Agents,
				eligible_agents = Eligible, aging_factor = Aging} = State) ->
	lager:info("call request from Agent ~p", [AgentPid]),
	State2 = case get_top_priority(Queue) of
		{none, NewQ} ->
			lager:info("no calls in queue, adding agent to free agent list"),
			agent:assess_call(AgentPid, self(), none),
			FreeAgents = case lists:member({AgentLogin, AgentPid}, Agents) of
							true -> RealAgents = Agents -- [{AgentLogin, AgentPid}],
									[{AgentLogin, AgentPid}|RealAgents];
							false -> [{AgentLogin, AgentPid}|Agents]
						end,
			State#state{free_agents = FreeAgents, queue = NewQ};
		{{TopKey, TopValue}, NewQ} ->
			lager:info("sending top priority call in queue"),
			agent:assess_call(AgentPid, self(), Aging, TopKey, TopValue),
			State#state{queue = NewQ}
	end,
	NewEligible = case lists:member({AgentLogin, AgentPid}, Eligible) of
				false -> [{AgentLogin, AgentPid}|Eligible];
				true -> Eligible
			end,
	{noreply, State2#state{eligible_agents = NewEligible}};

handle_cast({remove_call, CallPid}, #state{queue = Queue} = State) when is_pid(CallPid) ->
	NewQ = case find_by_pid(CallPid, Queue) of
		{Key, _Value} -> try_delete(Key, Queue);
		_ ->
			lager:info("Unable to find call with key ~p to be removed", [CallPid]),
			Queue
	end,
	EndTimer = get_expire_timer(NewQ, State),
	{noreply, State#state{queue = NewQ, end_timer = EndTimer}};

handle_cast({remove_call, Key}, #state{queue = Queue} = State) ->
	NewQ = try_delete(Key, Queue),
	EndTimer = get_expire_timer(NewQ, State),
	{noreply, State#state{queue = NewQ, end_timer = EndTimer}};

handle_cast({remove_cook, CookPid}, State) ->
	lager:info("Stopping cook ~p", [CookPid]),
	catch unlink(CookPid),
	catch cook:stop(CookPid),
	{noreply, State};

handle_cast({assign_call, AgentPid}, #state{queue = Queue} = State) ->
	NewState = case get_top_priority(Queue) of
		{none, NewQ} ->
			lager:info("No call to assign to ~p, re-notifying", [AgentPid]),
			agent:notify_call(AgentPid),
			State#state{queue = NewQ};
		{{TopKey, TopValue}, NewQ} ->
			try gen_media:ring(TopValue#queued_call.media, AgentPid, TopValue, ?getRingout) of
				ok ->
					lager:info("ring result is ok, removing call from queue"),
					remove_key_from_queue(NewQ, TopKey, State);
				deferred ->
					lager:info("ring result is deferred, removing call from queue"),
					remove_key_from_queue(NewQ, TopKey, State);
				ignored ->
					lager:info("ring result is ignored, removing call from queue"),
					remove_key_from_queue(NewQ, TopKey, State);
				Res ->
					lager:info("ring result is ~p", [Res]),
					{Priority, Ts} = TopKey,
					Callrec = gen_media:get_call(TopValue#queued_call.media),
					State2 = remove_key_from_queue(NewQ, TopKey, State#state{queue = NewQ}),
					requeue(Priority, Ts, TopValue#queued_call.media, TopValue#queued_call.cook, Callrec, State2)
			catch
				exit:{normal, Reason} ->
					lager:info("caught exit normal with reason ~p in vqueue ~p", [Reason, self()]),
					agent:notify_call(AgentPid),
					remove_key_from_queue(NewQ, TopKey, State);
				exit:{noproc, Reason} ->
					lager:info("caught exit noproc with reason ~p in vqueue ~p", [Reason, self()]),
					agent:notify_call(AgentPid),
					remove_key_from_queue(NewQ, TopKey, State)
			end
	end,
	{noreply, NewState};

handle_cast({remove_agent, AgentPid, AgentLogin}, #state{free_agents = Agents} = State) ->
	NewAgents = Agents -- [{AgentLogin, AgentPid}],
	{noreply, State#state{free_agents = NewAgents}};

handle_cast({remove_eligible, AgentPid, AgentLogin}, #state{eligible_agents = Agents} = State) ->
	NewAgents = Agents -- [{AgentLogin, AgentPid}],
	{noreply, State#state{eligible_agents = NewAgents}};

handle_cast({requeue, {Priority, Timestamp}, CallPid, Callrec}, State) ->
	lager:info("requeuing call ~p in queue ~p", [CallPid, self()]),
	add(self(), Priority, Timestamp, CallPid, Callrec),
	{noreply, State};

handle_cast({stop, Reason}, State) ->
	{stop, Reason, State};

handle_cast(Msg, State) ->
	lager:debug("Unhandled cast ~p for ~p", [Msg, undefined_name]),
	{noreply, State}.

%% =====
%% handle_info
%% =====

%% @private
handle_info(timeout, State) ->
	Notified = State#state.notified,
	NewQ = case State#state.incarnation > 0 of
		false ->
			State#state.queue;
		true ->
			Start = ouc_time:now_ms(),
			lager:info("Waiting for gen_media new pids..."),
			get_notified(Notified, Start, State#state.queue)
	end,
	lager:info("verifying to agents..."),
	verify_to_agents(State#state.call_skills),
	lager:info("verification sent..."),
	{noreply, State#state{queue = NewQ, subscribed = [node()]}};

handle_info(queue_expired, State) ->
	lager:info("received queue_expired message! Stopping queue."),
	{stop, expired, State};

handle_info({new_vqueue_manager, Pid}, State) ->
	lager:info("Notifying new vqueue manager ~p of one's existence", [Pid]),
	reach_vqueue_manager:add_queue(self()),
	{noreply, State};

handle_info({shutdown, Node}, State) when Node == node() ->
	lager:info("received shutdown msg"),
	gproc:unreg({n, g, {reach_vqueue, State#state.call_skills, State#state.priority, State#state.aging_factor}}),
	lager:info("Informing eligible agents of queue shutdown."),
	[agent:remove_queue(AgentPid, self()) || {_AgentLogin, AgentPid} <- State#state.eligible_agents],
	lists:foreach(fun({_K,V}) when is_pid(V#call.cook) -> cook:stop(V#call.cook); (_) -> ok end, gb_trees:to_list(State#state.queue)),
	reach_vqueue_manager:remove_queue(self()),
	Key = {n, g, {reach_vqueue, State#state.call_skills, State#state.priority, State#state.aging_factor, State#state.incarnation + 1}},
	BackupNode = reach_vqueue_manager:get_backup(node(), Key),
	case BackupNode of
		none -> lager:info("No BackupNode, shutting down");
		_ -> lager:info("BackUpNode is ~p, performing shutdown procedures...", [BackupNode]),
			OwnNode = node(),
			case Node of
				OwnNode ->
					Locals = find_node_calls(State),
					NewState = State#state{incarnation = State#state.incarnation + 1, notified = length(Locals)},
					reach_vqueue:start_at(BackupNode, NewState),
					notify_calls(Key, BackupNode, Locals);
				_ ->
					Remote = find_node_calls(Node, State),
					notify_calls(Key, BackupNode, Remote)
			end
	end,
	set_cpx_mon(State, delete),
	shutdown_manager:completed(node(), self()),
	{noreply, State};

handle_info({shutdown, Node}, State) ->
	lager:info("remote node ~p shutting down, transferring calls from that node: ~p", [Node, State#state.queue]),
	% Queue = remove_node(Node, State#state.queue),
	% lager:info("new queue is ~p", [Queue]),
	Key = {n, g, {reach_vqueue, State#state.call_skills, State#state.priority, State#state.aging_factor, State#state.incarnation}},
	BackupNode = reach_vqueue_manager:get_backup(Node, Key),
	NewState = case BackupNode of
		none -> lager:info("No BackupNode, shutting down"),
				State;
		_ -> lager:info("BackUpNode is ~p, performing shutdown procedures...", [BackupNode]),
			% Remote = find_node_calls(Node, State),
			% notify_calls(Key, BackupNode, Remote)
			{NodeCalls, OtherCalls} = split_node_calls(Node, State),
			lager:info("NodeCalls : ~p~nOtherCalls: ~p", [NodeCalls, OtherCalls]),
			NewQueue = gb_trees:from_orddict(OtherCalls),
			F = fun({K, Value}) ->
					Pid = Value#queued_call.media,
					{Pid, {K, Value}} end,
			LimboCalls = lists:map(F, NodeCalls),
			GetPid = fun({_K2, Val}) ->
					Val#queued_call.media end,
			Remote = lists:map(GetPid, NodeCalls),
			notify_calls(Key, BackupNode, Remote),
			State#state{queue = NewQueue, limbo_calls = LimboCalls}
	end,
	set_cpx_mon(State, delete),
	shutdown_manager:completed(Node, self()),
	{noreply, NewState};

handle_info({gen_media_recovery, OldPid, NewPid}, State) ->
	Queue = State#state.queue,
	LimboCalls = State#state.limbo_calls,
	% NewQueue = case find_by_pid(OldPid, Queue) of
	% 	none -> lager:error("Cannot find old PID of transferred call"),
	% 			Queue;
	% 	{Key, Value} ->
	% 			NewQ = gb_trees:update(Key, Value#queued_call{media = NewPid, cook = undefined}, Queue),
	% 			lager:info("Change PID of call from ~p to ~p", [OldPid, NewPid]),
	% 			NewQ
	% end,
	NewState = case lists:keytake(OldPid, 1, LimboCalls) of
		{value, Call, OtherLimbo} ->
			lager:info("Returning Call ~p to queue with new pid ~p", [Call, NewPid]),
			{OldPid, {Key, Value}} = Call,
			NewValue = Value#queued_call{media = NewPid},
			NewQueue = gb_trees:insert(Key, NewValue, Queue),
			State#state{queue = NewQueue, limbo_calls = OtherLimbo};
		false -> State
	end,
	{noreply, NewState};

handle_info(Info, State) ->
	lager:debug("~p got info ~p", [undefined_name, Info]),
	{noreply, State}.

%% =====
%% terminate
%% =====

%% @private
terminate(Reason, State) when is_atom(Reason) andalso Reason =:= normal ->
	gproc:unreg({n, g, {reach_vqueue, State#state.call_skills, State#state.priority, State#state.aging_factor}}),
	lager:info("Informing eligible agents of queue termination."),
	[agent:remove_queue(AgentPid, self()) || {_AgentLogin, AgentPid} <- State#state.eligible_agents],
	lists:foreach(fun({_K,V}) when is_pid(V#call.cook) -> cook:stop(V#call.cook); (_) -> ok end, gb_trees:to_list(State#state.queue)),
	reach_vqueue_manager:remove_queue(self()),
	shutdown_manager:unsubscribe(node(), self()),
	set_cpx_mon(State, delete),
	ok;
terminate(Reason, _State) when is_atom(Reason) andalso Reason =:= shutdown ->
	ok;
terminate(Reason, State) ->
	lager:notice("~p unusual terminate: ~p", [undefined_name, Reason]),
	lager:info("Informing eligible agents of queue termination."),
	[agent:remove_queue(AgentPid, self()) || {_AgentLogin, AgentPid} <- State#state.eligible_agents],
	reach_vqueue_manager:remove_queue(self()),
	shutdown_manager:unsubscribe(node(), self()),
	set_cpx_mon(State, delete),
	ok.

%% =====
%% code_chnage
%% =====

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% =====
%% Internal Functions
%% =====

remove_key_from_queue(Queue, Key, State) ->
	NewQ = try_delete(Key, Queue),
	EndTimer = get_expire_timer(NewQ, State),
	State#state{queue = NewQ, end_timer = EndTimer}.

requeue(Priority, Timestamp, CallPid, CookPid, Callrec, State) when is_pid(CallPid) ->
	cancel_timer(State),
	lager:info("Trying to add call ~p to self ~p", [CallPid, self()]),
	Time = case Timestamp of
			undefined -> ouc_time:now_micro();
			Value -> Value
	end,
	Key = {Priority, Time},
	case CookPid of
		undefined -> ok;
		_ -> cook:set_queue(CookPid, self())
	end,
	NewState = queue_call(node(CallPid), CookPid, Callrec, Key, State),
	Agents2 = case State#state.free_agents of
		[] ->
			lager:info("No free agents"),
			[];
		Agents ->
			% notify(Agent),
			RealAgents = lists:reverse(Agents),
			[{TopLogin, TopAgent}|RemAgents] = RealAgents,
			lager:info("free agents in queue, notifying top agent ~p ~p", [TopLogin, TopAgent]),
			agent:notify_call(TopAgent),
			lists:reverse(RemAgents)
	end,
	NewState#state{end_timer = undefined, free_agents = Agents2}.

%% @private
queue_call(Node, Cookpid, Callrec, Key, State) ->
	Queuedrec = #queued_call{node = Node, media = Callrec#call.source, cook = Cookpid, id = Callrec#call.id, channel = Callrec#call.type, module = Callrec#call.source_module},
	NewSkills = lists:umerge(lists:sort(State#state.call_skills), lists:sort(Callrec#call.skills)),
	ExpandedSkills = expand_magic_skills(State, Callrec, NewSkills),
	Value = Queuedrec#queued_call{skills=ExpandedSkills},
	lager:info("Queued call ~p with skills ~p", [Callrec#call.source, ExpandedSkills]),
	try gb_trees:insert(Key, Value, State#state.queue) of
		Trees ->
			erlang:monitor(process, Callrec#call.source),
			set_cpx_mon(State#state{queue=Trees}),
			State#state{queue = Trees}
	catch
		error:{key_exists, _} ->
			case gb_trees:lookup(Key, State#state.queue) of
				{value, #queued_call{id=CallId}} when CallId == Queuedrec#queued_call.id ->
					lager:notice("Call ~p already exists in queue ~p with key ~p", [Queuedrec#queued_call.id, State#state.name, Key]),
					State;
				_ ->
					 lager:notice("Generating new key for call ~p", [Queuedrec#queued_call.id]),
					 {Priority, _Ts} = Key,
					 Time = ouc_time:now_micro(),
					 queue_call(Node, Cookpid, Callrec, {Priority, Time}, State)
			end
	end.

%% @private
-spec(set_cpx_mon/1 :: (State :: #state{}) -> 'ok').
set_cpx_mon(State) ->
	set_cpx_mon(State, ignore).

%% @private
-spec(set_cpx_mon/2 :: (State :: #state{}, pid() | atom() | ignore) -> 'ok').
set_cpx_mon(_State, delete) ->
	cpx_monitor:drop({queue, undefined_name});
set_cpx_mon(State, Watch) ->
	Key = {queue, undefined_name},
	MidDetails = [
		{calls, gb_trees:size(State#state.queue)}
	],
	Details = case gb_trees:is_empty(State#state.queue) of
		true ->
			MidDetails;
		false ->
			{{_, NowMs}, _} = gb_trees:smallest(State#state.queue),
			[{oldest, {timestamp, NowMs}} | MidDetails]
	end,
	cpx_monitor:set(Key, Details, Watch).


% return the {Key, Value} pair where Value#call.id == Needle or none
% ie:  lookup a call by ID, return the key in queue and the full call data
%% @private
-spec(find_key/2 :: (Needle :: string(), GbTree :: call_queue()) -> {call_key(), #queued_call{}} | 'none').
find_key(Needle, GbTree) ->
	find_key_(Needle, gb_trees:next(gb_trees:iterator(GbTree))).

%% @private
-spec(find_key_/2 :: (Needle :: string(), Iterator :: {call_key(), #queued_call{}, any()} | 'none') -> {call_key(), #queued_call{}} | 'none').
find_key_(Needle, {Key, #queued_call{id = Needle} = Value, _Iter}) ->
	{Key, Value};
find_key_(Needle, {_Key, _Value, Iter}) ->
	find_key_(Needle, gb_trees:next(Iter));
find_key_(_Needle, none) ->
	none.

% @private
% same as find_key, only searches by pid
-spec(find_by_pid/2 :: (Needle :: pid(), GbTree :: call_queue()) -> {call_key(), #queued_call{}} | 'none').
find_by_pid(Needle, GbTree) ->
	find_by_pid_(Needle, gb_trees:next(gb_trees:iterator(GbTree))).

%% @private
-spec(find_by_pid_/2 :: (Needle :: pid(), Interator :: {call_key(), #queued_call{}, any()} | 'none') -> {call_key(), #queued_call{}} | 'none').
find_by_pid_(Needle, {Key, #queued_call{media = Needle} = Value, _Iter}) ->
	{Key, Value};
find_by_pid_(Needle, {_Key, _Value, Iter}) ->
	find_by_pid_(Needle, gb_trees:next(Iter));
find_by_pid_(_Needle, none) ->
	none.

find_node_calls(State) ->
	find_node_calls(node(), State).
find_node_calls(Node, State) ->
	GbTree = State#state.queue,
	% GprocKey = {n, g, {reach_vqueue, State#state.call_skills, State#state.priority, State#state.aging_factor, State#state.incarnation + 1}},
	find_node_calls_(Node, gb_trees:next(gb_trees:iterator(GbTree)), []).

find_node_calls_(Node, {_Key, #queued_call{media = CallPid} = _Value, Iter}, Calls) ->
	NewCalls = case node(CallPid) of
		Node -> lager:info("Call ~p is local", [CallPid]),
				% spawn(fun() -> gen_media:transfer_to_node(CallPid, reach_node_monitor:find_next_node(), GprocKey) end),
				[CallPid|Calls];
		_ -> Calls
	end,
	find_node_calls_(Node, gb_trees:next(Iter), NewCalls);
find_node_calls_(_Node, none, Calls) ->
	Calls.

notify_calls(Key, BackupNode, Calls) when is_list(Calls) ->
	lager:info("transferring local calls ~p to backup node ~p with Key ~p", [Calls, BackupNode, Key]),
	[gen_media:transfer_to_node(CallPid, BackupNode, Key) || CallPid <- Calls].

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

verify_to_agents(Skills) ->
	Prospects = agent_manager:list(),
	lager:info("Possible Agents: ~p", [Prospects]),
	[agent:verify_queue(Agent#agent_cache.pid, self(), Skills) || {_, Agent} <- Prospects].

get_expire_timer(Queue, State) ->
	TimerRef = State#state.end_timer,
	case gb_trees:is_empty(Queue) of
		true ->
			lager:info("Queue empty, starting expire timer for queue ~p", [self()]),
			erlang:send_after(?QUEUE_TIMEOUT, self(), queue_expired);
		false -> TimerRef
	end.

cancel_timer(State) ->
	case State#state.end_timer of
		undefined -> ok;
		TimerRef -> erlang:cancel_timer(TimerRef),
					lager:info("Expire timer for queue ~p cancelled", [self()])
	end.

get_top_priority(Queue) ->
	case gb_trees:is_empty(Queue) of
		true -> {none, Queue};
		false ->
			{TopKey, TopValue} = gb_trees:smallest(Queue),
			Call = TopValue#queued_call.media,
			case util:is_process_alive(Call) of
				true -> {{TopKey, TopValue}, Queue};
				false -> NewQ = gb_trees:delete_any(TopKey, Queue),
						 get_top_priority(NewQ)
			end
	end.

get_notified(0, _Start, Queue) ->
	Queue;

get_notified(Ctr, Start, Queue) ->
	NowMs = ouc_time:now_ms(),
	TimeElapsed = NowMs - Start,
	case TimeElapsed > ?REQUEUE_TIMEOUT of
		false ->
			lager:info("Still waiting for ~p notifications...", [Ctr]),
			TimeRemaining  = ?REQUEUE_TIMEOUT - TimeElapsed,
			receive
				{gen_media_recovery, OldPid, NewPid} ->
					case find_by_pid(OldPid, Queue) of
						none -> lager:error("Cannot find old PID of transferred call"),
								get_notified(Ctr - 1, Start, Queue);
						{Key, Value} ->
								NewQ = gb_trees:update(Key, Value#queued_call{media = NewPid, cook = undefined}, Queue),
								lager:info("Change PID of call from ~p to ~p", [OldPid, NewPid]),
								NewQ
					end
			after
				TimeRemaining ->
					lager:info("Requeue of new calls timed out."),
					Queue
			end;
		true ->
			lager:info("Requeue of new calls timed out."),
					Queue
	end.

try_delete(Key, Queue) ->
	case gb_trees:is_empty(Queue) of
		true -> Queue;
		false -> gb_trees:delete_any(Key, Queue)
	end.

split_node_calls(Node, State) ->
	QList = gb_trees:to_list(State#state.queue),
	F = fun({_Key, Value}) ->
			Value#queued_call.node == Node end,
	lists:partition(F, QList).

% remove_node(Node, Queue) ->
% 	List = gb_trees:to_list(Queue),
% 	F = fun({_, QCall}) ->
% 			case QCall#queued_call.node of
% 				Node -> false;
% 				_ -> true
% 			end
% 		end,
% 	NewList = lists:filter(F, List),
% 	gb_trees:from_orddict(NewList).

subscribe(Node, Subs) ->
	case lists:member(Node, Subs) of
		true -> Subs;
		false -> shutdown_manager:subscribe(Node, self()),
				[Node|Subs]
	end.

% init_gproc_prop(State) ->
% 	Prop = get_call_queue_prop(State),
% 	gproc:reg({p, g, cpx_call_queue}, Prop).

% get_call_queue_prop(State) ->
% 	#cpx_call_queue_prop{name = undefined_name, skills = State#state.call_skills}.

% offer_call([], _Call) ->
% 	[];
% offer_call([Agent|Tail], Call) ->
% 	case gen_media:ring(Call#call.source, Agent, Call, ?getRingout) of
% 		ok -> Agent;
% 		invalid -> offer_call(Tail, Call)
% 	end.
