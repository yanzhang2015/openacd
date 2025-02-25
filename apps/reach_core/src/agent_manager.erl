%%	The contents of this file are subject to the Common Public Attribution
%%	License Version 1.0 (the “License”); you may not use this file except
%%	in compliance with the License. You may obtain a copy of the License at
%%	http://opensource.org/licenses/cpal_1.0. The License is based on the
%%	Mozilla Public License Version 1.1 but Sections 14 and 15 have been
%%	added to cover use of software over a computer network and provide for
%%	limited attribution for the Original Developer. In addition, Exhibit A
%%	has been modified to be consistent with Exhibit B.
%%
%%	Software distributed under the License is distributed on an “AS IS”
%%	basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%	License for the specific language governing rights and limitations
%%	under the License.
%%
%%	The Original Code is OpenACD.
%%
%%	The Initial Developers of the Original Code is
%%	Andrew Thompson and Micah Warren.
%%
%%	All portions of the code written by the Initial Developers are Copyright
%%	(c) 2008-2009 SpiceCSM.
%%	All Rights Reserved.
%%
%%	Contributor(s):
%%
%%	Andrew Thompson <andrew at hijacked dot us>
%%	Micah Warren <micahw at lordnull dot com>
%%

%% @doc Centralized authority on where agent_fsm's are and starting them.
%% Similar in function to the {@link queue_manager}, just oriented towards
%% agents.  There can be only one `agent_manager' per node.

-module(agent_manager).
-author(micahw).
-behaviour(gen_leader).

-ifdef(TEST).
-define(STANDARD_TEST, true).
-include_lib("eunit/include/eunit.hrl").
-endif.
-ifdef(PROFILE).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("call.hrl").
-include("agent.hrl").

-type(agent_pid() :: pid()).
-type(time_avail() :: integer()).
-type(channels() :: [channel_type()]).
-type(endpoints() :: [atom()]). % list of media modules.
-type(agent_cache() :: {agent_pid(), agent_id(), time_avail(), skills(),
	channels(), endpoints()}).
-type(agent_entry() :: {#agent_key{}, #agent_cache{}}).

%-type(rotations() :: non_neg_integer()).
%-type(all_skill_flag() :: 'a' | 'z'). % a < z, a meaning it has the all flag,
% z lacking.  So, the gb tree will sort the a's first.
%-type(skill_list_length() :: non_neg_integer()).
%-type(went_avail_at() :: {pos_integer(), non_neg_integer(), non_neg_integer()}).
%-type(sort_key() :: {rotations(), all_skill_flag(), skill_list_length(), went_avail_at()}).

-record(state, {
	agents = dict:new() :: dict(),
	route_list = gb_trees:empty() :: gb_tree(),
	lists_requested = 0 :: integer(),
	max_agents_number :: integer()
}).

-type(state() :: #state{}).
-define(GEN_LEADER, true).
-include("gen_spec.hrl").

-define(has_all(Skills), case lists:member('_all', Skills) of true -> a; false -> z end).

% API exports
-export([
	start_link/1,
	start_link/2,
	start/1,
	start/2,
	stop/0,
	start_agent/1,
	start_agent_by_auth/1,
	start_agent_by_login/1,
	start_agent_by_pstn_login/3,
	query_agent/1,
	are_license_seats_available/0,
	update_skill_list/2,
	find_by_skill/1,
	find_avail_agents_by_skill/1,
	sort_agents_by_elegibility/1,
	filtered_route_list/1,
	filtered_route_list/3,
	find_by_pid/1,
	blab/2,
	get_leader/0,
	list/0,
	list_local/0,
	notify/5,
	set_avail/2,
	set_ends/2,route_list/0
]).

% gen_leader callbacks
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

%% API

%% @doc Starts the `agent_manger' linked to the calling process.
-spec(start_link/1 :: (Nodes :: [atom()]) -> {'ok', pid()}).
start_link(Nodes) ->
	gen_leader:start_link(?MODULE, Nodes, [{heartbeat, 1}, {vardir, util:run_dir()}], ?MODULE, [], []).

-spec(start_link/2 :: (Nodes :: [atom()], Opts :: [any()]) -> {'ok', pid()}).
start_link(Nodes, Opts) ->
	gen_leader:start_link(?MODULE, Nodes, [{heartbeat, 1}, {vardir, util:run_dir()}], ?MODULE, Opts, []).

%% @doc Starts the `agent_manager' without linking to the calling process.
-spec(start/1 :: (Nodes :: [atom()]) -> {'ok', pid()}).
start(Nodes) ->
	gen_leader:start(?MODULE, Nodes, [{heartbeat, 1}, {vardir, util:run_dir()}], ?MODULE, [], []).

-spec(start/2 :: (Nodes :: [atom()], Opts :: [any()]) -> {'ok', pid()}).
start(Nodes, Opts) ->
	gen_leader:start(?MODULE, Nodes, [{heartbeat, 1}, {vardir, util:run_dir()}], ?MODULE, Opts, []).

%% @doc stops the `agent_manager'.
-spec(stop/0 :: () -> {'ok', pid()}).
stop() ->
	gen_leader:call(?MODULE, stop).

%% @doc starts a new agent_fsm for `Agent'. Returns `{ok, pid()}', where `Pid' is the new agent_fsm pid.
-spec(start_agent/1 :: (Agent :: #agent{}) -> {'ok', pid()} | {'exists', pid()}).
start_agent(#agent{login = ALogin} = Agent) ->
	case query_agent(ALogin) of
		false ->
			case are_license_seats_available() of
				true ->
					gen_leader:call(?MODULE, {start_agent, Agent}, infinity);
				false ->
					{error, not_allowed}
			end;
		{true, Apid} ->
			{exists, Apid}
	end.

start_agent_by_auth(#agent_auth{id=Id, login=Username, skills=Skills,
		security_level=Security, profile=Profile, endpoints=Endpoints}) ->
	R = start_agent(#agent{id = Id, login = Username,
		skills = Skills, profile = Profile,
		security_level = Security}),
	case R of
		{ok, Pid} ->
			%% @todo -- should be part of agent start-up
			agent:set_endpoints(Pid, Endpoints);
		_ ->
			ok
	end,
	R.

start_agent_by_login(Login) ->
	case agent_auth:get_agent_by_login(Login) of
		{ok, Auth} ->
			start_agent_by_auth(Auth);
		_ ->
			{error, noagent}
	end.

start_agent_by_pstn_login(Login, Pass, Endpoints) ->
  case agent_auth:auth(Login, Pass) of
    {ok, Auth} when is_record(Auth, agent_auth) ->
      start_agent_by_auth(Auth#agent_auth{endpoints = Endpoints});
    _ ->
      {error, noagent}
  end.


%% @doc updates the skill-list cached here.  'Tis a case to prevent blockage.
-spec(update_skill_list/2 :: (Login :: string(), Skills :: skills()) -> 'ok').
update_skill_list(Login, Skills) ->
	gen_leader:cast(?MODULE, {update_skill_list, Login, Skills}).

%% @doc Locally find all available agents with a particular skillset that contains the subset `Skills'.
-spec(find_avail_agents_by_skill/1 :: (Skills :: [atom()]) -> [{#agent_key{}, #agent_cache{}}]).
find_avail_agents_by_skill(Skills) ->
	%lager:debug("skills passed:  ~p.", [Skills]),
	List = list_avail(),
	filter_avail_agents_by_skill(List, Skills).

%% @doc Locally find all available agents with a particular skillset, and
%% makes sure the server knows it's for routing.  Depricated in favor of
%% {@link filtered_route_list/3}
-spec(filtered_route_list/1 :: (Skills :: [atom()]) ->
		[agent_entry()]).
filtered_route_list(Skills) ->
	Agents = route_list(),
	filter_avail_agents_by_skill(Agents, Skills).

%% @doc Locally find all available agents with a particular skillset,
%% media channel, and media callback module.  Also makes sure the server
%% knows it's for routing.
-spec(filtered_route_list/3 :: (
	Skills :: [atom()],
	Chan :: atom(),
	Endpoint :: atom()) ->
		[agent_entry()]).
filtered_route_list(Skills, Chan, Endpoint) ->
	Agents = route_list(),
	[ O ||
		{_K, #agent_cache{skills = AgSkills, channels = AgChans, endpoints = AgEndpoints}} = O <- Agents,
		length(AgChans) > 0,
		(lists:member('_all', AgSkills) orelse lists:member('_all', Skills) )
		orelse
		util:list_contains_all(AgSkills, Skills),
		lists:member(Chan, AgChans),
		lists:member(Endpoint, AgEndpoints)
	].

%% @doc Filter the agents based on the skill list and availability.
-spec(filter_avail_agents_by_skill/2 :: (Agents :: [agent_entry()], Skills :: [atom()]) -> [agent_entry()]).
filter_avail_agents_by_skill(Agents, Skills) ->
	[O ||
		{_K, #agent_cache{skills = AgSkills} = AgCache} = O <- Agents,
		length(AgCache#agent_cache.channels) > 0,
		matches_agent_skills(AgSkills, Skills)].

%% @doc Sorted by idle time, then the length of the list of skills the agent has;  this means idle time is less important.
%% No un-idle agents should be in the list, otherwise it is fail.
-spec(sort_agents_by_elegibility/1 :: (Agents :: [agent_cache()]) -> [agent_cache()]).
sort_agents_by_elegibility(AvailSkilledAgents) ->
	lists:keysort(1, AvailSkilledAgents).

%help_sort(E1, E2) when size(E1) == 4, size(E2) == 4 ->
%	help_sort(erlang:append_element(E1, ignored), erlang:append_element(E2, ignored));
%help_sort({_K1, _V1, Time1, Skills1, _}, {_K2, _V2, Time2, Skills2, _}) ->
%	case {lists:member('_all', Skills1), lists:member('_all', Skills2)} of
%		{true, false} ->
%			false;
%		{false, true} ->
%			true;
%		_Else ->
%			if
%				length(Skills1) == length(Skills2) ->
%					Time1 =< Time2;
%				true ->
%					length(Skills1) =< length(Skills2)
%			end
%	end.

%% @doc Gets all the agents have have the given `[atom()] Skills'.
-spec(find_by_skill/1 :: (Skills :: [atom()]) -> [{string(), #agent_cache{}}]).
find_by_skill(Skills) ->
	Agents = list(),
	[O ||
		{_K, #agent_cache{skills = AgSkills}} = O <- Agents,
		matches_agent_skills(AgSkills, Skills)].

%% @doc Gets the login associated with the passed pid().
-spec(find_by_pid/1 :: (Apid :: pid()) -> string() | 'notfound').
find_by_pid(Apid) ->
	gen_leader:leader_call(?MODULE, {get_login, Apid}).

%% @doc Get a list of agents on all running nodes
-spec(list/0 :: () -> [agent_entry()]).
list() ->
	gen_leader:leader_call(?MODULE, list_agents).

%% @doc Get a list of agents on the node this `agent_manager' is running on.
-spec(list_local/0 :: () -> [agent_entry()]).
list_local() ->
	gen_leader:call(?MODULE, list_agents).

%% @doc List the available agents on this node.
-spec(list_avail/0 :: () -> [agent_entry()]).
list_avail() ->
	List = gen_leader:call(?MODULE, list_avail_agents),
	[X || {_, #agent_cache{channels = Chans}} = X <- List, length(Chans) > 0].

%% @doc Get a list of agents, tagged for how many requests of this type
%% have been made without the agent list changing in any way.  A change is
%% either an agent added, dropped, or skill list change.
-spec(route_list/0 :: () -> [agent_entry()]).
route_list() ->
	gen_leader:call(?MODULE, route_list_agents).

%% @doc Check if an agent idetified by agent record or login name string of `Login' exists
-spec(query_agent/1 ::	(Agent :: #agent{}) -> {'true', pid()} | 'false';
						(Login :: string()) -> {'true', pid()} | 'false').
query_agent(#agent{login=Login}) ->
	query_agent(Login);
query_agent(Login) ->
	gen_leader:leader_call(?MODULE, {exists, Login}).

are_license_seats_available() ->
  gen_leader:leader_call(?MODULE, check_license_seats).

%% @doc Notify the agent_manager of a local agent after an agent manager restart.
%% TODO update to take channels and endpoints too
-spec(notify/5 :: (Login :: string(), Id :: string(), Pid :: pid(), TimeAvail :: non_neg_integer(), Skills :: [atom()]) -> 'ok').
notify(Login, Id, Pid, TimeAvail, Skills) ->
	case gen_leader:call(?MODULE, {exists, Login}) of
		false ->
			gen_leader:call(?MODULE, {notify, Login, Id, Pid, TimeAvail, Skills});
		{true, Pid} ->
			ok;
		{true, _OtherPid} ->
			erlang:error({duplicate_registration, Login})
	end.

%% @doc Returns `{ok, pid()}' where `pid()' is the pid of the leader process.
-spec(get_leader/0 :: () -> {'ok', pid()}).
get_leader() ->
	gen_leader:leader_call(?MODULE, get_pid).

%% @doc Send the message `string() Text' to all agents that match the given filter.
-spec(blab/2 :: (Text :: string(), {Key :: 'agent' | 'node' | 'profile', Value :: string() | atom()}) -> 'ok').
blab(Text, all) ->
	gen_leader:leader_cast(?MODULE, {blab, Text, all});
blab(Text, {Key, Value}) ->
	gen_leader:leader_cast(?MODULE, {blab, Text, {Key, Value}}).

-spec(set_avail/2 :: (AgentLogin :: string(), Channels :: [atom()]) -> 'ok').
set_avail(AgentLogin, Channels) ->
	gen_leader:cast(?MODULE, {set_avail, AgentLogin, Channels}).

-spec(set_ends/2 :: (AgentLogin :: string(), Endpoints :: [atom()]) -> 'ok').
set_ends(AgentLogin, Endpoints) ->
	gen_leader:cast(?MODULE, {set_ends, AgentLogin, Endpoints}).

%% =================================================================
%% gen_leader callbacks
%% =================================================================

%% @hidden
init(_Opts) ->
	lager:debug("~p starting at ~p", [?MODULE, node()]),
	process_flag(trap_exit, true),

	try agent_auth:start() of
		Whatever ->
			lager:debug("building agent_state table:  ~p", [Whatever]),
			ok
	catch
		What:Why ->
			lager:warning("What:  ~w; Why:  ~p", [What, Why]),
			ok
	end,

	{ok, #state{}}.

%% @hidden
elected(State, _Election, Node) ->
	lager:info("elected by ~w", [Node]),
	mnesia:subscribe(system),
	{ok, ok, State}.

%% @hidden
%% if an agent is started at two locations, the 2nd notify will be told that
%% the register failed, allowing the agent to hagurk at that point.
surrendered(#state{agents = Agents} = State, _LeaderState, _Election) ->
	lager:info("surrendered", []),
	mnesia:unsubscribe(system),

	% clean out non-local pids
	F = fun(_Login, #agent_cache{pid = Apid}) ->
		node() =:= node(Apid)
	end,
	Locals = dict:filter(F, Agents),
	% and tell the leader about local pids
	Notify = fun({Login, V}) ->
		gen_leader:leader_cast(?MODULE, {update_notify, Login, V})
	end,
	Dictlist= dict:to_list(Locals),
	lists:foreach(Notify, Dictlist),
	RoutlistFilter = fun({_Key, #agent_cache{pid = Pid}}) ->
		node() =:= node(Pid)
	end,
	Routelist = gb_trees_filter(RoutlistFilter, State#state.route_list),
	{ok, State#state{agents=Locals, route_list = Routelist}}.

%% @hidden
handle_DOWN(Node, #state{agents = Agents} = State, _Election) ->
	lager:info("Node ~p died", [Node]),
	% clean out the pids associated w/ the dead node
	F = fun(_Login, #agent_cache{pid = Apid}) ->
		Node =/= node(Apid)
	end,
	Agents2 = dict:filter(F, Agents),
	Routelist = gb_trees_filter(fun({_Key, #agent_cache{pid = Pid}}) ->
		Node =/= node(Pid)
	end, State#state.route_list),
	{ok, State#state{agents = Agents2, route_list = Routelist}}.

%% @hidden
handle_leader_call(list_agents, _From, #state{agents = Agents} = State, _Election) ->
	{reply, dict:to_list(Agents), State};
handle_leader_call({exists, Agent}, From, #state{agents = _Agents} = State, Election) when is_list(Agent) ->
	%%lager:debug("Trying to determine if ~p exists", [Agent]),
	case handle_leader_call({full_data, Agent}, From, State, Election) of
		{reply, false, _State} = O ->
			O;
		{reply, #agent_cache{pid = Apid}, NewState} ->
			{reply, {true, Apid}, NewState}
	end;
handle_leader_call({full_data, Agent}, _From, #state{agents = Agents} = State, _Election) when is_list(Agent) ->
	case dict:find(Agent, Agents) of
		error ->
			%% TODO - a nasty hack for when the agent has a @ sign in their username
			case dict:find(re:replace(Agent, "_", "@", [{return, list}]), Agents) of
				error ->
					{reply, false, State};
				{ok, Value} ->
					{reply, Value, State}
			end;
		{ok, Value} ->
			{reply, Value, State}
	end;
handle_leader_call(check_license_seats, _From, #state{agents = Agents} = State, _Election) ->
  MaxAgentsNumber = case State#state.max_agents_number of
    undefined ->
      Res = agent_auth:get_license_seats(),
      Number = case Res of
        {ok, N} -> N;
        none ->
          lager:warning("No setting found for limiting the number of concurrent agents, considering unlimited", []),
          -1
      end,
      Number;
    Other -> Other
  end,
  lager:info("Number of agents vs. license limit: ~p / ~p", [dict:size(Agents), MaxAgentsNumber]),
  Reply = MaxAgentsNumber =:= -1 orelse dict:size(Agents) < MaxAgentsNumber,
  {reply, Reply, State#state{max_agents_number = MaxAgentsNumber}};
handle_leader_call(get_pid, _From, State, _Election) ->
	{reply, {ok, self()}, State};
handle_leader_call({get_login, Apid}, _From, #state{agents = Agents} = State, _Election) when is_pid(Apid) ->
	List = dict:to_list(Agents),
	Out = find_via_pid(Apid, List),
	{reply, Out, State};
handle_leader_call({get_login, Id}, _From, #state{agents = Agents} = State, _Election) ->
	List = dict:to_list(Agents),
	Out = find_via_id(Id, List),
	{reply, Out, State};
handle_leader_call(Message, From, State, _Election) ->
	lager:warning("received unexpected leader_call ~p from ~p", [Message, From]),
	{reply, ok, State}.

%% @hidden
handle_leader_cast({notify, Agent, AgentCache}, #state{agents = Agents} = State, _Election) ->
	lager:info("Notified of ~p (~p)", [Agent, AgentCache]),
	Apid = AgentCache#agent_cache.pid,
	case dict:find(Agent, Agents) of
		error ->
			lager:debug("Adding agent ~p", [Agent]),
			Agents2 = dict:store(Agent, AgentCache, Agents),
			Routelist = gb_trees:enter(#agent_key{
				rotations = 0,
				has_all = ?has_all(AgentCache#agent_cache.skills),
				skill_count = length(AgentCache#agent_cache.skills),
				idle_time = AgentCache#agent_cache.time_avail},
			AgentCache, State#state.route_list),
			{noreply, State#state{agents = Agents2, route_list = Routelist}};
		{ok, #agent_cache{pid = Apid}} ->
			lager:debug("Agent ~p already know", [Agent]),
			{noreply, State};
		_Else ->
			lager:debug("agent pid mismatch, telling imposter", []),
			agent:register_rejected(AgentCache#agent_cache.pid),
			{noreply, State}
	end;
handle_leader_cast({update_notify, Login, #agent_cache{pid = Pid} = Value}, #state{agents = Agents} = State, _Election) ->
	NewAgents = dict:update(Login, fun(_Old) -> Value end, Value, Agents),
	Midroutelist = gb_trees_filter(fun({_Key, #agent_cache{pid = Apid}}) ->
		Apid =/= Pid
	end, State#state.route_list),
	Skills = Value#agent_cache.skills,
	Time = Value#agent_cache.time_avail,
	Routelist = gb_trees:enter(#agent_key{ rotations = 0,
		has_all = ?has_all(Skills), skill_count = length(Skills),
		idle_time = Time}, Value, Midroutelist),
	{noreply, State#state{agents = NewAgents, route_list = Routelist}};
handle_leader_cast({notify_down, Agent}, #state{agents = Agents} = State, _Election) ->
	lager:notice("leader notified of ~p exiting", [Agent]),
	Routelist = case dict:find(Agent, Agents) of
		error ->
			State#state.route_list;
		{ok, #agent_cache{pid = Pid}} ->
			gb_trees_filter(fun({_Key, #agent_cache{pid = Apid}}) ->
				Apid =/= Pid
			end, State#state.route_list)
	end,
	{noreply, State#state{agents = dict:erase(Agent, Agents), route_list = Routelist}};
handle_leader_cast({blab, Text, {agent, Value}}, #state{agents = Agents} = State, _Election) ->
	case dict:find(Value, Agents) of
		error ->
			% /shrug.  Meh.
			{noreply, State};
		{ok, #agent_cache{pid = Apid}} ->
			agent:blab(Apid, Text),
			{noreply, State}
	end;
handle_leader_cast({blab, Text, {node, Value}}, #state{agents = Agents} = State, _Election) ->
	Alist = dict:to_list(Agents),
	F = fun({_Aname, #agent_cache{pid = Apid}}) ->
		case node(Apid) of
			Value ->
				agent:blab(Apid, Text);
			_Else -> ok
		end
	end,
	spawn(fun() -> lists:foreach(F, Alist) end),
	{noreply, State};
handle_leader_cast({blab, Text, {profile, Value}}, #state{agents = Agents} = State, _Election) ->
	Alist = dict:to_list(Agents),
	Foreach = fun({_Aname, #agent_cache{pid = Apid}}) ->
		try agent:dump_state(Apid) of
			#agent{profile = Value} ->
				agent:blab(Apid, Text);
			_Else ->
				ok
		catch
			What:Why ->
				lager:warning("could not blab to ~p.  ~p:~p", [Apid, What, Why]),
				ok
		end
	end,
	F = fun() ->
		lists:foreach(Foreach, Alist)
	end,
	spawn(F),
	{noreply, State};
handle_leader_cast({blab, Text, all}, #state{agents = Agents} = State, _Election) ->
	F = fun(_, #agent_cache{pid = Pid}) ->
		agent:blab(Pid, Text)
	end,
	spawn(fun() -> dict:map(F, Agents) end),
	{noreply, State};
handle_leader_cast(dump_election, State, Election) ->
	lager:debug("Dumping leader election.~nSelf:  ~p~nDump:  ~p", [self(), Election]),
	{noreply, State};
handle_leader_cast(Message, State, _Election) ->
	lager:warning("received unexpected leader_cast ~p", [Message]),
	{noreply, State}.

%% @hidden
from_leader(_Msg, State, _Election) ->
	lager:debug("Stub from leader.", []),
	{ok, State}.

%% @hidden
handle_call(list_agents, _From, #state{agents = Agents} = State, _Election) ->
	{reply, dict:to_list(Agents), State};
handle_call(list_avail_agents, _From, State, _Election) ->
	{reply, gb_trees:to_list(State#state.route_list), State};
handle_call(route_list_agents, _From, #state{agents = _Agents, lists_requested = Count, route_list = Routelist} = State, _Election) ->
	List = gb_trees:to_list(Routelist),
	NewRoutelist = case gb_trees:is_empty(Routelist) of
		true ->
			Routelist;
		false ->
			{#agent_key{rotations = OldCount} = Key, Val, Midroutelist} =
				gb_trees:take_smallest(Routelist),
			gb_trees:enter(Key#agent_key{rotations = OldCount + 1}, Val, Midroutelist)
	end,
	{reply, List, State#state{lists_requested = Count + 1, route_list = NewRoutelist}};
handle_call(stop, _From, State, _Election) ->
	{stop, normal, ok, State};
handle_call({start_agent, #agent{login = ALogin, id=Aid} = InAgent}, _From, #state{agents = Agents} = State, Election) ->
	% This should not be called directly!  use the wrapper start_agent/1
	Agent = InAgent#agent{release_data = ?DEFAULT_RELEASE},
	lager:info("Starting new agent ~p", [Agent]),
	{ok, Apid} = agent:start(Agent, [logging, gen_leader:candidates(Election)]),
	link(Apid),
	Value = #agent_cache{
		pid = Apid,
		id = Aid,
		time_avail = os:timestamp(),
		skills = Agent#agent.skills,
		channels = [],
		endpoints = dict:fetch_keys(Agent#agent.endpoints)
	},
	Leader = gen_leader:leader_node(Election),
	case node() of
		Leader ->
			ok;
		_ ->
			gen_leader:leader_cast(?MODULE, {update_notify, ALogin, Value})
	end,
	Agents2 = dict:store(ALogin, Value, Agents),
	dispatch_manager:end_avail(Apid),
	Key = #agent_key{
		has_all = ?has_all(Agent#agent.skills),
		skill_count = length(Agent#agent.skills),
		idle_time = os:timestamp()
	},
	RouteList = gb_trees:enter(Key, Value, State#state.route_list),
	{reply, {ok, Apid}, State#state{agents = Agents2, route_list = RouteList}};
handle_call({exists, Login}, _From, #state{agents = Agents} = State, Election) ->
	Leader = gen_leader:leader_node(Election),
	case dict:find(Login, Agents) of
		error when Leader =/= node() ->
			case gen_leader:leader_call(?MODULE, {full_data, Login}) of
				false ->
					{reply, false, State};
				#agent_cache{pid = Pid} = V when node(Pid) =:= node() ->
					% leader knows about a local agent, but we didn't!
					% So we update the local dict
					Agents2 = dict:store(Login, V, Agents),
					{reply, {true, Pid}, State#state{agents = Agents2}};
				#agent_cache{pid = OtherPid} ->
					{reply, {true, OtherPid}, State}
			end;
		error -> % we're the leader
			{reply, false, State};
		{ok, #agent_cache{pid = Pid}} ->
			{reply, {true, Pid}, State}
	end;

handle_call({notify, Login, #agent_cache{pid = Pid} = AgentCache}, _From, #state{agents = Agents} = State, Election) when is_pid(Pid) andalso node(Pid) =:= node() ->
	case dict:find(Login, Agents) of
		error ->
			link(Pid),
			case gen_leader:leader_node(Election) =:= node() of
				false ->
					gen_leader:leader_cast(?MODULE, {notify, Login, AgentCache});
				_Else ->
					ok
			end,
			Agents2 = dict:store(Login, AgentCache, Agents),
			#agent_cache{skills = Skills, time_avail = TimeAvail} = AgentCache,
			Midroutelist = gb_trees:enter(#agent_key{rotations = 0,
				has_all = ?has_all(Skills), skill_count = length(Skills),
				idle_time = TimeAvail}, AgentCache, State#state.route_list),
			{reply, ok, State#state{agents = Agents2, lists_requested = 0, route_list = clear_rotates(Midroutelist)}};
		{ok, {Pid, _Id}} ->
			{reply, ok, State};
		{ok, _OtherPid} ->
			% TODO - wait, so we get notified about an agent, we have a record of the an agent with that login already,
			% and we just say 'ok'?
			{reply, ok, State}
	end;
handle_call(Message, From, State, _Election) ->
	lager:warning("received unexpected call ~p from ~p", [Message, From]),
	{reply, ok, State}.


%% @hidden
handle_cast({set_avail, Nom, Chans}, #state{agents = Agents} = State, Election) ->
	Node = node(),
	#agent_cache{pid = Pid} = AgentCache = dict:fetch(Nom, Agents),
	Midroutelist = gb_trees_filter(fun({_Key, #agent_cache{pid = Apid}}) ->
		Apid =/= Pid
	end, State#state.route_list),
	%% If the new channel list is shorter, they likely went on call
	%% if so, we can consider the 'time they have been idle' as how long
	%% it's been since they took a call.
	% TODO This can be gamed by agents by bouncing idle and released
	Time = case length(AgentCache#agent_cache.channels) > length(Chans) of
		true -> os:timestamp();
		false -> AgentCache#agent_cache.time_avail
	end,
	#agent_cache{skills = Skills} = Out = AgentCache#agent_cache{time_avail = Time, channels = Chans},
	Key = #agent_key{
		rotations = 0,
		has_all = ?has_all(Skills),
		skill_count = length(Skills),
		idle_time = Time
	},
	Routelist = gb_trees:enter(Key, Out, Midroutelist),
	F = fun(_) ->
		case gen_leader:leader_node(Election) of
			Node ->
				ok;
			_ ->
				gen_leader:leader_cast(?MODULE, {update_notify, Nom, Out})
		end,
		Out
	end,
	NewAgents = dict:update(Nom, F, Agents),
	%lager:info("Updated key ~p to ~p", [Key, Out]),
	%lager:debug("rl:  ~p", [Routelist]),
	{noreply, State#state{agents = NewAgents, lists_requested = 0, route_list = clear_rotates(Routelist)}};

handle_cast({set_ends, Nom, Ends}, #state{agents = Agents} = State, Election) ->
	Node = node(),
	#agent_cache{ pid = Pid, id = Id, time_avail = Time, skills = Skills,
		channels = Chans} = dict:fetch(Nom, Agents),
	Midroutelist = gb_trees_filter(fun({_Key, #agent_cache{pid = Apid}}) ->
		Apid =/= Pid
	end, State#state.route_list),
	Out = #agent_cache{pid = Pid, id = Id, time_avail = Time,
		skills = Skills, channels = Chans, endpoints = Ends},
	Routelist = gb_trees:enter(#agent_key{ rotations = 0,
		has_all = ?has_all(Skills), skill_count = length(Skills),
		idle_time = Time}, Out, Midroutelist),
	F = fun(_) ->
		case gen_leader:leader_node(Election) of
			Node ->
				ok;
			_ ->
				gen_leader:leader_cast(?MODULE, {update_notify, Nom, Out})
		end,
		Out
	end,
	NewAgents = dict:update(Nom, F, Agents),
	{noreply, State#state{agents = NewAgents, lists_requested = 0, route_list = clear_rotates(Routelist)}};

%handle_cast({end_avail, Nom}, #state{agents = Agents} = State, Election) ->
%	Node = node(),
%	{Pid, Id, _, Skills} = dict:fetch(Nom, Agents),
%	Routelist = gb_trees_filter(fun({_, {Apid, _, _}}) ->
%		Apid =/= Pid
%	end, State#state.route_list),
%	NewAgents = dict:store(Nom, {Pid, Id, 0, Skills}, Agents),
%	case gen_leader:leader_node(Election) of
%		Node ->
%			ok;
%		_ ->
%			gen_leader:leader_cast(?MODULE, {update_notify, Nom, {Pid, Id, 0, Skills}})
%	end,
%	{noreply, State#state{agents = NewAgents, lists_requested = 0, route_list = Routelist}};
handle_cast({update_skill_list, Login, Skills}, #state{agents = Agents} = State, Election) ->
	Node = node(),
	#agent_cache{pid = Pid, time_avail = Time} = AgentCache = dict:fetch(Login, Agents),
	Out = AgentCache#agent_cache{skills = Skills},
	Midroutelist = clear_rotates(gb_trees_filter(fun({_, #agent_cache{pid = Apid}}) ->
		Apid =/= Pid
	end, State#state.route_list)),
	Routelist = gb_trees:enter(#agent_key{ rotations = 0,
		has_all = ?has_all(Skills), skill_count = length(Skills),
		idle_time = Time}, Out, Midroutelist),
	F = fun(_) ->
		case gen_leader:leader_node(Election) of
			Node ->
				ok;
			_ ->
				gen_leader:leader_cast(?MODULE, {update_notify, Login, Out})
		end,
		Out
	end,
	NewAgents = dict:update(Login, F, Agents),
	{noreply, State#state{agents = NewAgents, lists_requested = 0, route_list = Routelist}};
handle_cast(_Request, State, _Election) ->
	lager:debug("Stub handle_cast", []),
	{noreply, State}.

%% @hidden
handle_info({'EXIT', Pid, Reason}, #state{agents=Agents} = State) ->
	lager:notice("Caught exit for ~p with reason ~p", [Pid, Reason]),
	F = fun(Key, #agent_cache{ pid = Value, id = Id}) ->
		case Value =/= Pid of
			true -> true;
			false ->
				lager:notice("notifying leader of ~p exit", [{Key, Id}]),
				cpx_monitor:drop({agent, Id}),
				gen_leader:leader_cast(?MODULE, {notify_down, Key}),
				false
		end
	end,
	Routelist = clear_rotates(gb_trees_filter(fun({_, #agent_cache{pid = Apid}}) ->
		Apid =/= Pid
	end, State#state.route_list)),
	{noreply, State#state{agents=dict:filter(F, Agents), lists_requested = 0, route_list = Routelist}};
handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
	lager:warning("mnesia down at ~w", [Node]),
	{noreply, State};
handle_info(Msg, State) ->
	lager:debug("Stub handle_info for ~p", [Msg]),
	{noreply, State}.

%% @hidden
terminate(Reason, _State) ->
	lager:notice("Terminating:  ~p", [Reason]),
	ok.

%% @hidden
code_change(_OldVsn, State, _Election, _Extra) ->
	{ok, State}.

%% @private
find_via_pid(_Needle, []) ->
	notfound;
find_via_pid(Needle, [{Key, #agent_cache{pid = Needle}} | _Tail]) ->
	Key;
find_via_pid(Needle, [{_Key, _NotNeedle} | Tail]) ->
	find_via_pid(Needle, Tail).

%% @private
find_via_id(_Needle, []) ->
	notfound;
find_via_id(Needle, [{Key, #agent_cache{id = Needle}} | _Tail]) ->
	Key;
find_via_id(Needle, [_Head | Tail]) ->
	find_via_id(Needle, Tail).

%% @doc returns a new tree which contians only entrys where Fun({Key, Val}) = true.
gb_trees_filter(Fun, Tree) ->
	Itor = gb_trees:iterator(Tree),
	gb_trees_filter(Fun, gb_trees:next(Itor), Tree).

gb_trees_filter(_Fun, none, Tree) ->
	Tree;
gb_trees_filter(Fun, {Key, Val, Itor}, Tree) ->
	case Fun({Key, Val}) of
		true ->
			gb_trees_filter(Fun, gb_trees:next(Itor), Tree);
		false ->
			Newtree = gb_trees:delete_any(Key, Tree),
			gb_trees_filter(Fun, gb_trees:next(Itor), Newtree)
	end.

clear_rotates({#agent_key{rotations = 0} = Key, Val, Tree}) ->
	gb_trees:enter(Key, Val, Tree);
clear_rotates({Key, Val, Tree}) ->
	Newtree = gb_trees:enter(Key#agent_key{rotations = 0}, Val, Tree),
	clear_rotates(gb_trees:take_largest(Newtree));
clear_rotates(Tree) ->
	case gb_trees:is_empty(Tree) of
		true ->
			Tree;
		false ->
			clear_rotates(gb_trees:take_largest(Tree))
	end.

matches_agent_skills(AgSkills, Skills) ->
	% check if either the call or the agent has the _all skill
	(lists:member('_all', AgSkills) orelse lists:member('_all', Skills))
		% if there's no _all skill, make sure the agent has all the required skills
		orelse util:list_contains_all(AgSkills, Skills).

-ifdef(STANDARD_TEST).

ds() ->
	spawn(fun() -> ok end).

filter_avail_agents_by_skill_test_() ->
	[{"one in, one out",
	fun() ->
		Agents = [{{agent_key, 0, z, 1, {100, 100, 100}}, {agent_cache, ds(), "agent", 0, [skill], [dummy], []}}],
		?assertEqual(Agents, filter_avail_agents_by_skill(Agents, [skill]))
	end},
	{"two in, one out",
	fun() ->
		[Out | _] = Agents = [
			{{agent_key, 0, z, 1, {100, 100, 100}}, {agent_cache, ds(), "agent1", 0, [skill], [voice], []}},
			{{agent_key, 0, z, 0, {100, 100, 100}}, {agent_cache, ds(), "agent2", 0, [], [voice], []}}
		],
		?assertEqual([Out], filter_avail_agents_by_skill(Agents, [skill]))
	end},
	{"agent with all gets in",
	fun() ->
		Agents = [{{agent_key, 0, z, 1, {100, 100, 100}}, {agent_cache, ds(), "agent", 0, ['_all'], [dummy], []}}],
		?assertEqual(Agents, filter_avail_agents_by_skill(Agents, [skill]))
	end},
	{"agents get through when all passed in",
	fun() ->
		Agents = [
			{{agent_key, 0, z, 1, {100, 100, 100}}, {agent_cache, ds(), "agent1", 0, [skill], [dummy], []}},
			{{agent_key, 0, z, 0, {100, 100, 100}}, {agent_cache, ds(), "agent2", 0, [], [dummy], []}}
		],
		?assertEqual(Agents, filter_avail_agents_by_skill(Agents, ['_all']))
	end}].

-record(election, {
	leader = none             :: 'none' | pid(),
	previous_leader = none    :: 'none' | pid(),
	name,
	leadernode = none         :: node(),
	candidate_nodes = []      :: [node()],
	worker_nodes = []         :: [node()],
	down = []                 :: [node()],
	monitored = [],
	buffered = [],
	seed_node = none          :: 'none' | node(),
	status,
	elid,
	acks = []                 :: [node()],
	work_down = []            :: [node()],
	cand_timer_int            :: integer(),
	cand_timer                :: term(),
	pendack                   :: node(),
	incarn,
	nextel                    :: integer(),
	%% all | one. When `all' each election event
	%% will be broadcast to all candidate nodes.
	bcast_type
}).

handle_cast_test_() ->
	{setup,
	fun() ->
		Election = #election{
			leader = node(),
			leadernode = node()
		},
		State = #state{},
		{State, Election}
	end,
	fun(_) ->
		ok
	end,
	fun({Seedstate, Election}) ->
		[{"basic now availalble",
		fun() ->
			Agents = dict:from_list([{"agent", {agent_cache, ds(), "agent", 0, [], [], []}}]),
			State = #state{agents = Agents},
			{noreply, Newstate} = handle_cast({set_avail, "agent", [voice]}, State, Election),
			?assertNot(gb_trees:is_empty(Newstate#state.route_list)),
			?assertMatch([{{agent_key, 0, z, 0, _}, {agent_cache, _, "agent", _, [], [voice], []}}], gb_trees:to_list(Newstate#state.route_list))
		end},
		{"basic end avail",
		fun() ->
			Time = os:timestamp(),
			Pid = ds(),
			Agents = dict:from_list([{"agent", {agent_cache, Pid, "agent", Time, [], [voice], []}}]),
			Routelist = gb_trees:enter({agent_key, 0, z, 0, Time}, {agent_cache, Pid, "agent", Time, [], [voice], []}, gb_trees:empty()),
			State = Seedstate#state{agents = Agents, route_list = Routelist},
			{noreply, Newstate} = handle_cast({set_avail, "agent", []}, State, Election),
			?assertMatch([{{agent_key, 0, z, 0, _}, {agent_cache, _, "agent", _, [], [], []}}], gb_trees:to_list(Newstate#state.route_list))
		end},
		{"updatin' a skill list of an idle agent",
		fun() ->
			Pid = ds(),
			Time = os:timestamp(),
			Agents = dict:from_list([{"agent", {agent_cache, Pid, "agent", Time, [], [dummy], [dummy]}}]),
			Routelist = gb_trees:enter({agent_key, 0, z, 0, Time}, {agent_cache, Pid, "agent", Time, [], [dummy], [dummy]}, gb_trees:empty()),
			State = #state{agents = Agents, route_list = Routelist},
			{noreply, NewState} = handle_cast({update_skill_list, "agent", [skill]}, State, Election),
			?assertNot(gb_trees:is_empty(NewState#state.route_list)),
			?assertMatch([{{agent_key, 0, z, 1, _}, {agent_cache, Pid, "agent", _, [skill], [dummy], [dummy]}}], gb_trees:to_list(NewState#state.route_list))
		end}]
	end}.

external_api_test_() ->
	{setup, fun() ->
		meck:new(gen_leader)
	end,
	fun(_) ->
		meck:validate(gen_leader),
		meck:unload(gen_leader)
	end,
	fun(_) -> [

		{"start_link/1", fun() ->
			meck:expect(gen_leader, start_link, fun(?MODULE, [Node], [{heartbeat, 1}, {vardir, _RunDir}], ?MODULE, [], []) ->
				?assertEqual(node(), Node),
				{ok, util:zombie()}
			end),
			?assertMatch({ok, _Pid}, start_link([node()]))
		end},

		{"start_link/2", fun() ->
			meck:expect(gen_leader, start_link, fun(?MODULE, [Node], [{heartbeat, 1}, {vardir, _Rundir}], ?MODULE, opts, []) ->
				?assertEqual(node(), Node),
				{ok, util:zombie()}
			end),
			?assertMatch({ok, _Pid}, start_link([node()], opts))
		end},

		{"start/1", fun() ->
			meck:expect(gen_leader, start, fun(?MODULE, [Node], [{heartbeat, 1}, {vardir, _Rundir}], ?MODULE, [], []) ->
				?assertEqual(node(), Node),
				{ok, util:zombie()}
			end),
			?assertMatch({ok, _Pid}, start([node()]))
		end},

		{"start/2", fun() ->
			meck:expect(gen_leader, start, fun(?MODULE, [Node], [{heartbeat, 1}, {vardir, _Rundir}], ?MODULE, opts, []) ->
				?assertEqual(node(), Node),
				{ok, util:zombie()}
			end),
			?assertMatch({ok, _Pid}, start([node()], opts))
		end},

		{"stop/0", fun() ->
			meck:expect(gen_leader, call, fun(?MODULE, stop) ->
				ok
			end),
			?assertEqual(ok, stop())
		end},

		{"start_agent/1, agent exists", fun() ->
			meck:expect(gen_leader, leader_call, fun(?MODULE, {exists, "testagent"}) ->
				{true, util:zombie()}
			end),
			Agent = #agent{login = "testagent"},
			?assertMatch({exists, _Pid}, start_agent(Agent))
		end},

		{"start_agent/1, agent doesn't exist", fun() ->
			Agent = #agent{login = "testagent"},
			meck:expect(gen_leader, leader_call, fun(?MODULE, {exists, "testagent"}) ->
				false
			end),
			meck:expect(gen_leader, call, fun(?MODULE, {start_agent, InAgent}, infinity) ->
				?assertEqual(Agent, InAgent),
				{'ok', util:zombie()}
			end),
			?assertMatch({ok, _Pid}, start_agent(Agent))
		end},

		{"query_agent/1, given record", fun() ->
			meck:expect(gen_leader, leader_call, fun(?MODULE, {exists, "testagent"}) ->
				ok
			end),
			Agent = #agent{login = "testagent"},
			?assertEqual(ok, query_agent(Agent))
		end},

		{"query_agent/1, given login", fun() ->
			meck:expect(gen_leader, leader_call, fun(?MODULE, {exists, "testagent"}) ->
				ok
			end),
			?assertEqual(ok, query_agent("testagent"))
		end},

		{"update_skill_list/2", fun() ->
			meck:expect(gen_leader, cast, fun(?MODULE, {update_skill_list, "testagent", [english, awesome]}) ->
				ok
			end),
			?assertEqual(ok, update_skill_list("testagent", [english, awesome]))
		end},

		{"find_avail_agents_by_skill/1, 'english' skill given", fun() ->
			AgentCaches = [
				{"key1", #agent_cache{channels = [voice], skills = [], id = "skip1"}},
				{"key2", #agent_cache{channels = [], skills = [english], id = "skip2"}},
				{"key3", #agent_cache{time_avail = 1, channels = [voice], skills = [english], id = "gotit1"}},
				{"key4", #agent_cache{time_avail = 1, channels = [voice], skills = ['_all'], id = "gotit2"}}
			],
			meck:expect(gen_leader, call, fun(?MODULE, list_avail_agents) ->
				AgentCaches
			end),
			Expected = [
				{"key3", #agent_cache{time_avail = 1, channels = [voice], skills = [english], id = "gotit1"}},
				{"key4", #agent_cache{time_avail = 1, channels = [voice], skills = ['_all'], id = "gotit2"}}
			],
			?assertEqual(Expected, find_avail_agents_by_skill([english]))
		end},

		{"find_avail_agents_by_skill/1, '_all' skill given", fun() ->
			AgentCaches = [
				{"key1", #agent_cache{time_avail = 1, channels = [voice], skills = [], id = "skip1"}},
				{"key2", #agent_cache{time_avail = 1, channels = [], skills = [english], id = "skip2"}},
				{"key3", #agent_cache{time_avail = 1, channels = [voice], skills = [english], id = "gotit1"}},
				{"key4", #agent_cache{time_avail = 1, channels = [voice], skills = ['_all'], id = "gotit2"}}
			],
			meck:expect(gen_leader, call, fun(?MODULE, list_avail_agents) ->
				AgentCaches
			end),
			Expected = [
				{"key1", #agent_cache{time_avail = 1, channels = [voice], skills = [], id = "skip1"}},
				{"key3", #agent_cache{time_avail = 1, channels = [voice], skills = [english], id = "gotit1"}},
				{"key4", #agent_cache{time_avail = 1, channels = [voice], skills = ['_all'], id = "gotit2"}}
			],
			?assertEqual(Expected, find_avail_agents_by_skill(['_all']))
		end},

		{"filtered_route_list/3", fun() ->
			AgentList = [
				{"key1", #agent_cache{skills = [], channels = [], endpoints = [], id = "skip1"}},
				{"key2", #agent_cache{skills = ['_all'], channels = [], endpoints = [], id = "skip2"}},
				{"key3", #agent_cache{skills = [english], channels = [], endpoints = [], id = "skip3"}},
				{"key4", #agent_cache{skills = [], channels = [voice], endpoints = [], id = "skip4"}},
				{"key5", #agent_cache{skills = [], channels = [], endpoints = [dummy_media], id = "skip5"}},
				{"key6", #agent_cache{time_avail = 1, skills = ['_all'], channels = [voice], endpoints = [dummy_media], id = "take6"}},
				{"key7", #agent_cache{time_avail = 1, skills = [english], channels = [voice], endpoints = [dummy_media], id = "take7"}}
			],
			meck:expect(gen_leader, call, fun(?MODULE, route_list_agents) ->
				AgentList
			end),
			Expected = [
				{"key6", #agent_cache{time_avail = 1, skills = ['_all'], channels = [voice], endpoints = [dummy_media], id = "take6"}},
				{"key7", #agent_cache{time_avail = 1, skills = [english], channels = [voice], endpoints = [dummy_media], id = "take7"}}
			],
			?assertEqual(Expected, filtered_route_list([english], voice, dummy_media))
		end},

		{"find_by_pid/1", fun() ->
			Zombie = util:zombie(),
			meck:expect(gen_leader, leader_call, fun(?MODULE, {get_login, Agent}) ->
				?assertEqual(Zombie, Agent),
				"zombie_guard"
			end),
			?assertEqual("zombie_guard", find_by_pid(Zombie))
		end},

		{"blab/2, all", fun() ->
			meck:expect(gen_leader, leader_cast, fun(?MODULE, {blab, "blab", all}) ->
				ok
			end),
			?assertEqual(ok, blab("blab", all))
		end},

		{"blab/2, {key, value}", fun() ->
			meck:expect(gen_leader, leader_cast, fun(?MODULE, {blab, "blab", {node, Node}}) ->
				?assertEqual(node(), Node),
				ok
			end),
			?assertEqual(ok, blab("blab", {node, node()}))
		end},

		{"get_leader/0", fun() ->
			Zombie = util:zombie(),
			meck:expect(gen_leader, leader_call, fun(?MODULE, get_pid) ->
				{ok, Zombie}
			end),
			?assertEqual({ok, Zombie}, get_leader())
		end},

		{"list/0", fun() ->
			meck:expect(gen_leader, call, fun(?MODULE, list_agents) ->
				[]
			end),
			?assertEqual([], list())
		end},

		{"set_avail/2", fun() ->
			meck:expect(gen_leader, cast, fun(?MODULE, {set_avail, "testagent", [voice]}) ->
				ok
			end),
			?assertEqual(ok, set_avail("testagent", [voice]))
		end},

		{"set_ends/2", fun() ->
			meck:expect(gen_leader, cast, fun(?MODULE, {set_ends, "testagent", [dummy_media]}) ->
				ok
			end),
			?assertEqual(ok, set_ends("testagent", [dummy_media]))
		end},

		{"route_list/0", fun() ->
			AgentList = [
				{"key1", #agent_cache{skills = [], channels = [], endpoints = [], id = "skip1"}},
				{"key2", #agent_cache{skills = ['_all'], channels = [], endpoints = [], id = "skip2"}},
				{"key3", #agent_cache{skills = [english], channels = [], endpoints = [], id = "skip3"}},
				{"key4", #agent_cache{skills = [], channels = [voice], endpoints = [], id = "skip4"}},
				{"key5", #agent_cache{skills = [], channels = [], endpoints = [dummy_media], id = "skip5"}},
				{"key6", #agent_cache{time_avail = 1, skills = ['_all'], channels = [voice], endpoints = [dummy_media], id = "take6"}},
				{"key7", #agent_cache{time_avail = 1, skills = [english], channels = [voice], endpoints = [dummy_media], id = "take7"}}
			],
			meck:expect(gen_leader, call, fun(?MODULE, route_list_agents) ->
				AgentList
			end),
			?assertEqual(AgentList, route_list())
		end}

	] end}.

internal_state_test_() -> [
	{"elected", fun() ->
		meck:new(mnesia),
		meck:expect(mnesia, subscribe, fun(system) -> ok end),
		?assertEqual({ok, ok, state}, elected(state, election, node())),
		?assertMatch([{_Pid, {mnesia, subscribe, [system]}, _}], meck:history(mnesia)),
		?assert(meck:validate(mnesia)),
		meck:unload(mnesia)
	end},

	{"surrendered", fun() ->
		meck:new(mnesia),
		meck:expect(mnesia, unsubscribe, fun(system) -> ok end),
		util:start_testnode(),
		{ok, Slave} = slave:start(net_adm:localhost(), agent_manager_surrendered),
		LocalZ = util:zombie(),
		RemoteZ = rpc:call(Slave, util, zombie, []),
		LocalA = #agent_cache{time_avail = 1, pid = LocalZ},
		RemoteA = #agent_cache{time_avail = 1, pid = RemoteZ},
		State = #state{
			agents = dict:from_list([
				{"local", LocalA},
				{"remote", RemoteA}
			]),
			route_list = gb_trees:from_orddict([
				{1, LocalA},
				{2, RemoteA}
			])
		},
		meck:new(gen_leader),
		meck:expect(gen_leader, leader_cast, fun(?MODULE, {update_notify, "local", InAgent}) ->
			?assertEqual(LocalA, InAgent)
		end),
		{ok, State0} = surrendered(State, "leaderstate", "election"),
		?assertEqual([{"local", LocalA}], dict:to_list(State0#state.agents)),
		?assertEqual([{1, LocalA}], gb_trees:to_list(State0#state.route_list)),
		?assert(meck:validate(mnesia)),
		?assert(meck:validate(gen_leader)),
		meck:unload(mnesia),
		meck:unload(gen_leader),
		slave:stop(Slave)
	end},

	{"handle_DOWN", fun() ->
		util:start_testnode(),
		{ok, Slave} = slave:start(net_adm:localhost(), agent_manager_handle_down),
		LocalZ = util:zombie(),
		RemoteZ = rpc:call(Slave, util, zombie, []),
		LocalA = #agent_cache{time_avail = 1, pid = LocalZ},
		RemoteA = #agent_cache{time_avail = 1, pid = RemoteZ},
		State = #state{
			agents = dict:from_list([
				{"local", LocalA},
				{"remote", RemoteA}
			]),
			route_list = gb_trees:from_orddict([
				{1, LocalA},
				{2, RemoteA}
			])
		},
		{ok, State0} = handle_DOWN(Slave, State, "election"),
		?assertEqual([{"local", LocalA}], dict:to_list(State0#state.agents)),
		?assertEqual([{1, LocalA}], gb_trees:to_list(State0#state.route_list)),
		slave:stop(Slave)
	end},

	{"handle_leader_call", [

		{"{exists, Agent}, agent exists", fun() ->
			State = #state{
				agents = dict:from_list([
					{"testagent", #agent_cache{pid = "pid"}}
				])
			},
			?assertEqual({reply, {true, "pid"}, State}, handle_leader_call({exists, "testagent"}, "from", State, "election"))
		end},

		{"{exists, Agent}, agent not found", fun() ->
			State = #state{ agents = dict:new() },
			?assertEqual({reply, false, State}, handle_leader_call({exists, "testagent"}, "from", State, "election"))
		end},

		{"{full_data, Agent}, agent not found", fun() ->
			State = #state{ agents = dict:new() },
			?assertEqual({reply, false, State}, handle_leader_call({full_data, "testagent"}, "from", State, "election"))
		end},

		%% the next is a hack for freeswitch.  sip registrations can't have
		%% two '@' in the name, so the sip endpoint does magic to usernames
		%% that have an @, turning it into an _.  When freesiwtch media wants
		%% to lookup the agent, it send agent_domain rather than agent@domain
		%% thus the fall back.
		{"{full_data, Agent}, agent lookup has _ instead of @", fun() ->
			State = #state{ agents = dict:from_list([
				{"testagent@example.com", "data"}
			]) },
			?assertEqual({reply, "data", State}, handle_leader_call({full_data, "testagent_example.com"}, "from", State, "election"))
		end},

		{"{full_data, Agent}, simple success", fun() ->
			State = #state{ agents = dict:from_list([ {"testagent", "data"} ]) },
			?assertEqual({reply, "data", State}, handle_leader_call({full_data, "testagent"}, "from", State, "election"))
		end},

		{"get_pid", fun() ->
			Self = self(),
			?assertEqual({reply, {ok, Self}, "state"}, handle_leader_call(get_pid, "from", "state", "election"))
		end},

		{"{get_login, Apid}, found", fun() ->
			Zombie = util:zombie(),
			Agent = #agent_cache{time_avail = 1, pid = Zombie, id = "agent"},
			State = #state{agents = dict:from_list([
				{"testagent", Agent}
			])},
			?assertEqual({reply, "testagent", State}, handle_leader_call({get_login, Zombie}, "from", State, "election"))
		end},

		{"{get_login, Apid}, not found", fun() ->
			State = #state{agents = dict:new()},
			?assertEqual({reply, notfound, State}, handle_leader_call({get_login, self()}, "from", State, "election"))
		end},

		{"{get_login, Id}, found", fun() ->
			Agent = #agent_cache{time_avail = 1, pid = self(), id = "agent"},
			State = #state{agents = dict:from_list([
				{"testagent", Agent}
			])},
			?assertEqual({reply, "testagent", State}, handle_leader_call({get_login, "agent"}, "from", State, "election"))
		end},

		{"{get_login, Id}, not found", fun() ->
			State = #state{agents = dict:new()},
			?assertEqual({reply, notfound, State}, handle_leader_call({get_login, "agent"}, "from", State, "election"))
		end}

	]},

	{"handle_leader_cast", [
		{"{notify, Agent, AgentCache}, simple success", fun() ->
			State = #state{agents = dict:new(), route_list = gb_trees:empty()},
			AgentCache = #agent_cache{pid = self(), id = "agentId",
				time_avail = 1, skills = [], channels = [], endpoints = []},
			AgentKey = #agent_key{rotations = 0, has_all = z, skill_count = 0,
				idle_time = 1},
			ExpectedState = #state{
				agents = dict:from_list([
					{"testagent", AgentCache}
				]),
				route_list = gb_trees:from_orddict([
					{AgentKey, AgentCache}
				])
			},
			?assertEqual({noreply, ExpectedState}, handle_leader_cast({notify, "testagent", AgentCache}, State, "election"))
		end},

		{"{notify, Agent, AgentCache}, agent already known", fun() ->
			AgentCache = #agent_cache{pid = self(), id = "agentId",
				time_avail = 1, skills = [], channels = [], endpoints = []},
			AgentKey = #agent_key{rotations = 0, has_all = z, skill_count = 0,
				idle_time = 1},
			ExpectedState = State = #state{
				agents = dict:from_list([
					{"testagent", AgentCache}
				]),
				route_list = gb_trees:from_orddict([
					{AgentKey, AgentCache}
				])
			},
			?assertEqual({noreply, ExpectedState}, handle_leader_cast({notify, "testagent", AgentCache}, State, "election"))
		end},

		{"{notify, Agent, AgentCache}, duplicate, pid mismatch", fun() ->
			AgentCache = #agent_cache{pid = self(), id = "agentId",
				time_avail = 1, skills = [], channels = [], endpoints = []},
			AgentKey = #agent_key{rotations = 0, has_all = z, skill_count = 0,
				idle_time = 1},
			State = #state{
				agents = dict:from_list([
					{"testagent", AgentCache}
				]),
				route_list = gb_trees:from_orddict([
					{AgentKey, AgentCache}
				])
			},
			Zombie = util:zombie(),
			BadCache = #agent_cache{pid = Zombie, id = "agentId",
				time_avail = 1, skills = [], channels = [], endpoints = []},
			meck:new(agent),
			meck:expect(agent, register_rejected, fun(InPid) ->
				?assertEqual(Zombie, InPid)
			end),
			?assertEqual({noreply, State}, handle_leader_cast({notify, "testagent", BadCache}, State, "election")),
			?assert(meck:validate(agent)),
			?assertMatch([{_Pid, {agent, register_rejected, [Zombie]}, _}],
				meck:history(agent)
			),
			meck:unload(agent)
		end},

		{"{update_notify, Login, AgentCache}", fun() ->
			AgentCache = #agent_cache{ pid = self(), id = "agentId",
				time_avail = 1, skills = [], channels = [], endpoints = []},
			Key = #agent_key{rotations = 0, has_all = z, skill_count = 0,
				idle_time = 1},
			State = #state{agents = dict:new(), route_list = gb_trees:empty()},
			Expected = #state{
				agents = dict:from_list([
					{"testagent", AgentCache}
				]),
				route_list = gb_trees:from_orddict([
					{Key, AgentCache}
				])
			},
			?assertEqual({noreply, Expected}, handle_leader_cast({update_notify, "testagent", AgentCache}, State, "election"))
		end},

		{"{notify_down, Agent}, agent not found", fun() ->
			State = #state{},
			?assertEqual({noreply, State}, handle_leader_cast({notify_down, "testagent"}, State, "election"))
		end},

		{"{notify_down, Agent}, simple success", fun() ->
			AgentCache = #agent_cache{id = "agentid", pid = self()},
			State = #state{
				agents = dict:from_list([
					{"testagent", AgentCache}
				]),
				route_list = gb_trees:from_orddict([
					{1, AgentCache}
				])
			},
			Expected = #state{},
			?assertEqual({noreply, Expected}, handle_leader_cast({notify_down, "testagent"}, State, "election"))
		end},

		{"{blab, Text, {agent, Value}}", fun() ->
			Zombie = util:zombie(),
			State = #state{agents = dict:from_list([{"testagent", #agent_cache{pid = Zombie}}])},
			meck:new(agent),
			meck:expect(agent, blab, fun(InPid, "blab blab") ->
				?assertEqual(Zombie, InPid)
			end),
			handle_leader_cast({blab, "blab blab", {agent, "testagent"}}, State, "election"),
			?assertEqual(1, length(meck:history(agent))),
			?assert(meck:validate(agent)),
			meck:unload(agent)
		end},

		{"{blab, Text, {node, Node}}", {timeout, 60, fun() ->
			util:start_testnode(),
			{ok, Slave} = slave:start(net_adm:localhost(), agent_manager_blab_node),
			RZombie = rpc:call(Slave, util, zombie, []),
			LZombie = util:zombie(),
			State = #state{agents = dict:from_list([
				{"remote_dude", #agent_cache{pid = RZombie}},
				{"local_dude", #agent_cache{pid = LZombie}}
			])},
			meck:new(agent),
			meck:expect(agent, blab, fun(InPid, "blab blab") ->
				?assertEqual(RZombie, InPid)
			end),
			handle_leader_cast({blab, "blab blab", {node, Slave}}, State, "election"),
			% spawns out the work, so wait for it to finish
			timer:sleep(10),
			?assertEqual(1, length(meck:history(agent))),
			?assert(meck:validate(agent)),
			meck:unload(agent),
			slave:stop(Slave)
		end}},

		{"{blab, Text, {profile, Value}}", fun() ->
			State = #state{agents = dict:from_list([
				{"dude1", #agent_cache{pid = true}},
				{"dude2", #agent_cache{pid = false}}
			])},
			meck:new(agent),
			meck:expect(agent, blab, fun(true, "blab blab") -> ok end),
			meck:expect(agent, dump_state, fun
				(true) -> #agent{login = "testagent", id = "id", profile = "testprofile"};
				(false) -> #agent{login = "tnegatset", id = "di", profile = "eliforptset"}
			end),
			handle_leader_cast({blab, "blab blab", {profile, "testprofile"}}, State, "election"),
			% work done in a spawn
			timer:sleep(10),
			?assert(meck:validate(agent)),
			?assertEqual(3, length(meck:history(agent))),
			meck:unload(agent)
		end},

		{"{blab, Text, all}", fun() ->
			State = #state{agents = dict:from_list([
				{"dude1", #agent_cache{pid = pid}},
				{"dude2", #agent_cache{pid = pid}}
			])},
			meck:new(agent),
			meck:expect(agent, blab, fun(pid, "blab blab") -> ok end),
			handle_leader_cast({blab, "blab blab", all}, State, "election"),
			timer:sleep(10),
			?assert(meck:validate(agent)),
			?assertEqual(2, length(meck:history(agent))),
			meck:unload(agent)
		end}

	]},

	{"handle_call", [

		{"list_agents", fun() ->
			?assertEqual({reply, [], #state{}}, handle_call(list_agents, "from", #state{}, "election"))
		end},

		{"list_avail_agents, empty tree", fun() ->
			?assertEqual({reply, [], #state{lists_requested = 1}}, handle_call(route_list_agents, "from", #state{}, "election"))
		end},

		{"list_avail_agents, populated tree", fun() ->
			InitList = [
				{#agent_key{rotations = 0, has_all = z, skill_count = 0, idle_time = 1}, "agent1"},
				{#agent_key{rotations = 0, has_all = z, skill_count = 0, idle_time = 2}, "agent2"}
			],
			Route1 = gb_trees:from_orddict(InitList),
			Route2 =  gb_trees:from_orddict([
				{#agent_key{rotations = 0, has_all = z, skill_count = 0, idle_time = 2}, "agent2"},
				{#agent_key{rotations = 1, has_all = z, skill_count = 0, idle_time = 1}, "agent1"}
			]),
			State = #state{route_list = Route1},
			% ExpectedState = #state{route_list = Route2, lists_requested = 1},
			{reply, OotList, OotState} = handle_call(route_list_agents, "from", State, "election"),
			?assertEqual(InitList, OotList),
			?assertEqual(gb_trees:to_list(Route2), gb_trees:to_list(OotState#state.route_list))
		end},

		{"stop", fun() ->
			?assertEqual({stop, normal, ok, state}, handle_call(stop, "from", state, "election"))
		end},

		% {"{start_agent, InAgent}", fun() ->
		% 	Mecks = [agent, gen_leader, dispatch_manager],
		% 	[meck:new(X) || X <- Mecks],
		% 	Zombie = util:zombie(),
		% 	meck:expect(agent, start, fun(_Rec, _Opts) ->
		% 		{ok, Zombie}
		% 	end),
		% 	meck:expect(gen_leader, candidates, fun(_) ->
		% 		[node()]
		% 	end),
		% 	meck:expect(gen_leader, leader_node, fun(_) -> node() end),
		% 	meck:expect(dispatch_manager, end_avail, fun(APid) ->
		% 		?assertEqual(Zombie, APid)
		% 	end),
		% 	InAgent = #agent{id = "agent1", login = "testagent"},
		% 	State = #state{},
		% 	{reply, _Preply, State0} = handle_call({start_agent, InAgent}, "from", State, "election"),
		% 	?assertEqual(1, gb_trees:size(State0#state.route_list)),
		% 	?assertEqual(1, dict:size(State0#state.agents)),
		% 	[begin
		% 		?assert(meck:validate(X)),
		% 		meck:unload(X)
		% 	end || X <- Mecks]
		% end},

		{"{exists, Login}, found local", fun() ->
			meck:new(gen_leader),
			meck:expect(gen_leader, leader_node, fun(_) -> node() end),
			Zombie = util:zombie(),
			State = #state{agents = dict:from_list([
				{"testagent", #agent_cache{pid = Zombie}}
			])},
			?assertEqual({reply, {true, Zombie}, State}, handle_call({exists, "testagent"}, "from", State, "election")),
			?assert(meck:validate(gen_leader)),
			meck:unload(gen_leader)
		end},

		{"{exists, Login}, not found", fun() ->
			meck:new(gen_leader),
			meck:expect(gen_leader, leader_node, fun(_) -> node() end),
			meck:expect(gen_leader, leader_call, fun(?MODULE, {full_data, "testagent"}) ->
				false
			end),
			?assertEqual({reply, false, #state{}}, handle_call({exists, "testagent"}, "from", #state{}, "election")),
			?assert(meck:validate(gen_leader)),
			meck:unload(gen_leader)
		end},

		{"{exists, Login}, leader found", fun() ->
			util:start_testnode(),
			{ok, Slave} = slave:start(net_adm:localhost(), agent_manager_exists),
			Zombie = rpc:call(Slave, util, zombie, []),
			meck:new(gen_leader),
			meck:expect(gen_leader, leader_node, fun(_) -> leader_node end),
			meck:expect(gen_leader, leader_call, fun(?MODULE, {full_data, "testagent"}) ->
				#agent_cache{pid = Zombie}
			end),
			?assertEqual({reply, {true, Zombie}, #state{}}, handle_call({exists, "testagent"}, "from", #state{}, "election")),
			?assert(meck:validate(gen_leader)),
			meck:unload(gen_leader),
			slave:stop(Slave)
		end},

		{"{exists, Login}, leader found, should be local", fun() ->
			Zombie = util:zombie(),
			meck:new(gen_leader),
			meck:expect(gen_leader, leader_node, fun(_) -> notus end),
			meck:expect(gen_leader, leader_call, fun(?MODULE, {full_data, "testagent"}) ->
				#agent_cache{pid = Zombie, time_avail = 1}
			end),
			ExpectedState = #state{agents = dict:from_list([
				{"testagent", #agent_cache{pid = Zombie, time_avail = 1}}
			])},
			?assertEqual({reply, {true, Zombie}, ExpectedState}, handle_call({exists, "testagent"}, "from", #state{}, "election")),
			?assert(meck:validate(gen_leader)),
			meck:unload(gen_leader)
		end},

		% {"{notify, Login, AgentCache}, not found, not leader", fun() ->
		% 	Zombie = util:zombie(),
		% 	AgentCache = #agent_cache{pid = Zombie, time_avail = 1, skills = []},
		% 	meck:new(gen_leader),
		% 	meck:expect(gen_leader, leader_node, fun(_) -> notus end),
		% 	meck:expect(gen_leader, leader_cast, fun(?MODULE, {notify, "testagent", InAgentCache}) ->
		% 		?assertEqual(AgentCache, InAgentCache)
		% 	end),
		% 	ExpectedState = #state{
		% 		agents = dict:from_list([
		% 			{"testagent", AgentCache}
		% 		]),
		% 		route_list = gb_trees:from_orddict([
		% 			{#agent_key{rotations = 0, has_all = z, skill_count = 0, idle_time = 1}, AgentCache}
		% 		])
		% 	},
		% 	?assertEqual({reply, ok, ExpectedState}, handle_call({notify, "testagent", AgentCache}, "from", #state{}, "election")),
		% 	?assert(meck:validate(gen_leader)),
		% 	?assertEqual(2, length(meck:history(gen_leader))),
		% 	meck:unload(gen_leader)
		% end},

		% {"{notify, Login, AgentCache}, not found, leader", fun() ->
		% 	Zombie = util:zombie(),
		% 	AgentCache = #agent_cache{pid = Zombie, time_avail = 1, skills = []},
		% 	meck:new(gen_leader),
		% 	meck:expect(gen_leader, leader_node, fun(_) -> node() end),
		% 	meck:expect(gen_leader, leader_cast, fun(?MODULE, {notify, "testagent", InAgentCache}) ->
		% 		?assertEqual(AgentCache, InAgentCache)
		% 	end),
		% 	ExpectedState = #state{
		% 		agents = dict:from_list([
		% 			{"testagent", AgentCache}
		% 		]),
		% 		route_list = gb_trees:from_orddict([
		% 			{#agent_key{rotations = 0, has_all = z, skill_count = 0, idle_time = 1}, AgentCache}
		% 		])
		% 	},
		% 	?assertEqual({reply, ok, ExpectedState}, handle_call({notify, "testagent", AgentCache}, "from", #state{}, "election")),
		% 	?assert(meck:validate(gen_leader)),
		% 	?assertEqual(1, length(meck:history(gen_leader))),
		% 	meck:unload(gen_leader)
		% end},

		% TODO look into what notify is actually for
		{"{notify, Login, AgentCache}, found, pid match", ?_assert(true)}
	]},

	{"handle_cast", [

		{"{set_avail, Nom, Chans}, chan list longer", fun() ->
			Zombie = util:zombie(),
			meck:new(gen_leader),
			meck:expect(gen_leader, leader_node, fun(_) -> node() end),
			AgentCache = #agent_cache{pid = Zombie, skills = [],
				time_avail = {0,0,0}, channels = []},
			AgentKey = #agent_key{rotations = 0, has_all = z, skill_count = 0,
				idle_time = {0, 0, 0}},
			State = #state{
				agents = dict:from_list([{"testagent", AgentCache}]),
				route_list = gb_trees:from_orddict([{AgentKey, AgentCache}])
			},
			{noreply, NewState} = handle_cast({set_avail, "testagent", [voice]}, State, "election"),
			?assert(meck:validate(gen_leader)),
			meck:unload(gen_leader),
			TestCache = dict:fetch("testagent", NewState#state.agents),
			{TestKey, _} = gb_trees:smallest(NewState#state.route_list),
			?assertEqual({0,0,0}, TestCache#agent_cache.time_avail),
			?assertEqual({0,0,0}, TestKey#agent_key.idle_time)
		end},

		{"{set_avail, Nom, Chans}, chan list shorter", fun() ->
			Zombie = util:zombie(),
			meck:new(gen_leader),
			meck:expect(gen_leader, leader_node, fun(_) -> node() end),
			AgentCache = #agent_cache{pid = Zombie, skills = [],
				time_avail = {0,0,0}, channels = [voice]},
			AgentKey = #agent_key{rotations = 0, has_all = z, skill_count = 0,
				idle_time = {0, 0, 0}},
			State = #state{
				agents = dict:from_list([{"testagent", AgentCache}]),
				route_list = gb_trees:from_orddict([{AgentKey, AgentCache}])
			},
			{noreply, NewState} = handle_cast({set_avail, "testagent", []}, State, "election"),
			?assert(meck:validate(gen_leader)),
			meck:unload(gen_leader),
			TestCache = dict:fetch("testagent", NewState#state.agents),
			{TestKey, _} = gb_trees:smallest(NewState#state.route_list),
			?assert({0,0,0} < TestCache#agent_cache.time_avail),
			?assert({0,0,0} < TestKey#agent_key.idle_time)
		end},

		{"{set_ends, Nom, Ends}, is leader", fun() ->
			Zombie = util:zombie(),
			OldCache = #agent_cache{id = "agentId", pid = Zombie, time_avail = 1,
				skills = [], channels = [], endpoints = [old_endpoint]},
			State = #state{agents = dict:from_list([{"testagent", OldCache}])},
			meck:new(gen_leader),
			meck:expect(gen_leader, leader_node, fun(_) -> node() end),
			{noreply, TestState} = handle_cast({set_ends, "testagent", [new_endpoint]}, State, "election"),
			{_, Val, _} = gb_trees:take_smallest(TestState#state.route_list),
			?assertEqual([new_endpoint], Val#agent_cache.endpoints),
			?assert(meck:validate(gen_leader)),
			meck:unload(gen_leader)
		end},

		{"{set_ends, Nom, Ends}, is not leader", fun() ->
			Zombie = util:zombie(),
			OldCache = #agent_cache{id = "agentId", pid = Zombie, time_avail = 1,
				skills = [], channels = [], endpoints = [old_endpoint]},
			State = #state{agents = dict:from_list([{"testagent", OldCache}])},
			meck:new(gen_leader),
			meck:expect(gen_leader, leader_node, fun(_) -> notus end),
			meck:expect(gen_leader, leader_cast, fun(?MODULE, {update_notify, _Nom, Out}) ->
				?assertNotEqual(OldCache, Out)
			end),
			{noreply, TestState} = handle_cast({set_ends, "testagent", [new_endpoint]}, State, "election"),
			{_, Val, _} = gb_trees:take_smallest(TestState#state.route_list),
			?assertEqual([new_endpoint], Val#agent_cache.endpoints),
			?assert(meck:validate(gen_leader)),
			?assertEqual(2, length(meck:history(gen_leader))),
			meck:unload(gen_leader)
		end},

		{"{update_skill_list, Login, Skills}, is leader", fun() ->
			Zombie = util:zombie(),
			OldCache = #agent_cache{id = "agentId", pid = Zombie, time_avail = 1,
				skills = [old_skill], channels = [], endpoints = []},
			State = #state{agents = dict:from_list([{"testagent", OldCache}])},
			meck:new(gen_leader),
			meck:expect(gen_leader, leader_node, fun(_) -> node() end),
			{noreply, TestState} = handle_cast({update_skill_list, "testagent", [new_skill]}, State, "election"),
			{_, Val, _} = gb_trees:take_smallest(TestState#state.route_list),
			?assertEqual([new_skill], Val#agent_cache.skills),
			?assert(meck:validate(gen_leader)),
			meck:unload(gen_leader)
		end},

		{"{update_skill_list, Login, Skills}, is not leader", fun() ->
			Zombie = util:zombie(),
			OldCache = #agent_cache{id = "agentId", pid = Zombie, time_avail = 1,
				skills = [old_skill], channels = [], endpoints = []},
			State = #state{agents = dict:from_list([{"testagent", OldCache}])},
			meck:new(gen_leader),
			meck:expect(gen_leader, leader_node, fun(_) -> notus end),
			meck:expect(gen_leader, leader_cast, fun(?MODULE, {update_notify, _Nom, Out}) ->
				?assertNotEqual(OldCache, Out)
			end),
			{noreply, TestState} = handle_cast({update_skill_list, "testagent", [new_skill]}, State, "election"),
			{_, Val, _} = gb_trees:take_smallest(TestState#state.route_list),
			?assertEqual([new_skill], Val#agent_cache.skills),
			?assert(meck:validate(gen_leader)),
			?assertEqual(2, length(meck:history(gen_leader))),
			meck:unload(gen_leader)
		end}
	]},

	{"handle_info", [
		{"{'EXIT', Pid, Reason}, Agent death", fun() ->
			Zombie = util:zombie(),
			meck:new(gen_leader),
			meck:new(cpx_monitor),
			meck:expect(gen_leader, leader_cast, fun(?MODULE, {notify_down, "zombie"}) -> ok end),
			meck:expect(cpx_monitor, drop, fun({agent, "a"}) -> ok end),
			State = #state{
				agents = dict:from_list([
					{"zombie", #agent_cache{id = "a", pid = Zombie}}
				]),
				route_list = gb_trees:from_orddict([
					{"key", #agent_cache{id = "a", pid = Zombie}}
				])
			},
			{noreply, TestState} = handle_info({'EXIT', Zombie, headshot}, State),
			?assertEqual(#state{}, TestState),
			[begin
				?assertEqual(1, length(meck:history(X))),
				?assert(meck:validate(X)),
				meck:unload(X)
			end || X <- [gen_leader, cpx_monitor]]
		end}
	]}

	].

% -record(multi_node_test_state, {
% 	master_node,
% 	slave_node,
% 	master_am,
% 	slave_am
% }).

% multi_node_test_() ->
% 	util:start_testnode(),
% 	Master = util:start_testnode(agent_manager_master),
% 	Slave = util:start_testnode(agent_manager_slave),
% 	mnesia:change_config(extra_db_nodes, [Master, Slave]),
% 	cover:start([Master, Slave]),
% 	{inorder, {foreach, fun() ->
% 		rpc:call(Master, mnesia, stop, []),
% 		rpc:call(Slave, mnesia, stop, []),
% 		mnesia:delete_schema([Master, Slave]),
% 		mnesia:create_schema([Master, Slave]),
% 		rpc:call(Master, mnesia, start, []),
% 		rpc:call(Slave, mnesia, start, []),
% 		mnesia:change_table_copy_type(schema, Master, disc_copies),
% 		mnesia:change_table_copy_type(schema, Slave, disc_copies),
% 		{ok, AMMaster} = rpc:call(Master, ?MODULE, start, [[Master, Slave]]),
% 		{ok, AMSlave} = rpc:call(Slave, ?MODULE, start, [[Master, Slave]]),
% 		#multi_node_test_state{
% 			master_node = Master,
% 			slave_node = Slave,
% 			master_am = AMMaster,
% 			slave_am = AMSlave
% 		}
% 	end,
% 	fun(_) ->
% 		rpc:call(Master, ?MODULE, stop, []),
% 		rpc:call(Slave, ?MODULE, stop, []),
% 		rpc:call(Master, mnesia, stop, []),
% 		rpc:call(Slave, mnesia, stop, [])
% 	end,
% 	[fun(_TestState) -> {"Slave skips added agent", fun() ->
% 		% only the leader knows about every agent, it seems
% 		% the reason not every manager needs to know about every
% 		% agent is the cook will ask each dispatcher, which will ask
% 		% the local manager.  The resulting lists are combined.
% 		Agent = #agent{id = "agent", login = "agent"},
% 		{ok, _Apid} = rpc:call(Master, ?MODULE, start_agent, [Agent]),
% 		List = rpc:call(Slave, ?MODULE, list, []),
% 		?assertEqual([], List)
% 	end} end,
% 	fun(_TestState) -> {"Master is informed of agent on slave", fun() ->
% 		Agent = #agent{id = "agent", login = "agent", skills = []},
% 		{ok, Apid} = rpc:call(Slave, ?MODULE, start_agent, [Agent]),
% 		receive after 100 -> ok end,
% 		List = rpc:call(Master, ?MODULE, list, []),
% 		?assertMatch([{"agent", #agent_cache{pid = Apid, id="agent", time_avail = {_T1, _T2, _T3}, skills = [], channels = _ChanList, endpoints = []}}], List)
% 	end} end,
% 	fun(TestState) -> {"Master removes agents from dead node", fun() ->
% 		Agent = #agent{id = "agent", login = "agent", skills = []},
% 		{ok, Apid} = rpc:call(Slave, ?MODULE, start_agent, [Agent]),
% 		List = rpc:call(Master, ?MODULE, list, []),
% 		?assertMatch([{"agent", #agent_cache{pid = Apid, id = "agent", time_avail = {_T1, _T2, _T3}, skills = [], channels = _ChanList, endpoints = []}}], List),
% 		rpc:call(Slave, erlang, exit, [TestState#multi_node_test_state.slave_am, kill]),
% 		receive after 100 -> ok end
% 		% TODO enable this at some point.
% 		%?assertEqual([], rpc:call(Master, ?MODULE, list, []))
% 	end} end]}}.


%		[
%			fun({Master, Slave, Agent, _Agent2}) ->
%				{"Slave picks up added agent",
%					fun() ->
%						{ok, Pid} = rpc:call(Master, ?MODULE, start_agent, [Agent]),
%						?assertMatch({exists, Pid}, rpc:call(Slave, ?MODULE, start_agent, [Agent]))
%					end
%				}
%			end,
%			fun({Master, Slave, Agent, _Agent2}) ->
%				{"Slave continues after master dies",
%					fun() ->
%						{ok, _Pid} = rpc:call(Master, ?MODULE, start_agent, [Agent]),
%						slave:stop(Master),
%						%rpc:call(Master, erlang, disconnect_node, [Slave]),
%						%rpc:call(Slave, erlang, disconnect_node, [Master]),
%						?assertMatch({ok, _NewPid}, rpc:call(Slave, ?MODULE, start_agent, [Agent]))
%					end
%				}
%			end,
%			fun({Master, Slave, _Agent, _Agent2}) ->
%				{"Slave becomes master after master dies",
%					fun() ->
%						%% getting the pids is important for this test
%						cover:stop([Master, Slave]),
%						slave:stop(Master),
%						slave:stop(Slave),
%
%						slave:start(net_adm:localhost(), master, " -pa debug_ebin"),
%						slave:start(net_adm:localhost(), slave, " -pa debug_ebin"),
%						cover:start([Master, Slave]),
%
%						{ok, _MasterP} = rpc:call(Master, ?MODULE, start, [[Master, Slave]]),
%						{ok, SlaveP} = rpc:call(Slave, ?MODULE, start, [[Master, Slave]]),
%
%						%% test proper begins
%						rpc:call(Master, erlang, disconnect_node, [Slave]),
%						cover:stop([Master]),
%						slave:stop(Master),
%
%						?assertMatch({ok, SlaveP}, rpc:call(Slave, ?MODULE, get_leader, []))
%
%						%?assertMatch(undefined, global:whereis_name(?MODULE)),
%						%?assertMatch({ok, _Pid}, rpc:call(Slave, ?MODULE, start_agent, [Agent])),
%						%?assertMatch({true, _Pid}, rpc:call(Slave, ?MODULE, query_agent, [Agent])),
%
%
%						%Globalwhere = global:whereis_name(?MODULE),
%						%Slaveself = rpc:call(Slave, erlang, whereis, [?MODULE]),
%
%						%?assertMatch(Globalwhere, Slaveself)
%					end
%				}
%			end,
%			fun({Master, Slave, Agent, Agent2}) ->
%				{"Net Split with unique agents",
%					fun() ->
%						{ok, Apid1} = rpc:call(Master, ?MODULE, start_agent, [Agent]),
%
%						?assertMatch({exists, Apid1}, rpc:call(Slave, ?MODULE, start_agent, [Agent])),
%
%						rpc:call(Master, erlang, disconnect_node, [Slave]),
%						rpc:call(Slave, erlang, disconnect_node, [Master]),
%
%						{ok, Apid2} = rpc:call(Slave, ?MODULE, start_agent, [Agent2]),
%
%						Pinged = rpc:call(Master, net_adm, ping, [Slave]),
%						Pinged = rpc:call(Slave, net_adm, ping, [Master]),
%
%						?assert(Pinged =:= pong),
%
%						?assertMatch({true, Apid1}, rpc:call(Slave, ?MODULE, query_agent, [Agent])),
%						?assertMatch({true, Apid2}, rpc:call(Master, ?MODULE, query_agent, [Agent2]))
%
%					end
%				}
%			end,
%			fun({Master, Slave, Agent, Agent2}) ->
%				{"Master removes agents for a dead node",
%					fun() ->
%						?assertMatch({ok, _Pid}, rpc:call(Slave, ?MODULE, start_agent, [Agent])),
%						?assertMatch({ok, _Pid}, rpc:call(Master, ?MODULE, start_agent, [Agent2])),
%						?assertMatch({true, _Pid}, rpc:call(Master, ?MODULE, query_agent, [Agent])),
%						%rpc:call(Master, erlang, disconnect_node, [Slave]),
%						cover:stop(Slave),
%						slave:stop(Slave),
%						?assertEqual(false, rpc:call(Master, ?MODULE, query_agent, [Agent])),
%						?assertMatch({true, _Pid}, rpc:call(Master, ?MODULE, query_agent, [Agent2])),
%						?assertMatch({ok, _Pid}, rpc:call(Master, ?MODULE, start_agent, [Agent]))
%					end
%				}
%			end,
%			fun({Master, Slave, Agent, _Agent2}) ->
%				{"Master is notified of agent removal on slave",
%					fun() ->
%						{ok, Pid} = rpc:call(Slave, ?MODULE, start_agent, [Agent]),
%						?assertMatch({true, Pid}, rpc:call(Slave, ?MODULE, query_agent, [Agent])),
%						?assertMatch({true, Pid}, rpc:call(Master, ?MODULE, query_agent, [Agent])),
%						exit(Pid, kill),
%						timer:sleep(300),
%						?assertMatch(false, rpc:call(Slave, ?MODULE, query_agent, [Agent])),
%						?assertMatch(false, rpc:call(Master, ?MODULE, query_agent, [Agent]))
%					end
%				}
%			end
%		]
%	}.
-endif.

-ifdef(PROFILE).

avg(Times) ->
	Sum = lists:foldl(fun(E, S) -> E + S end, 0, Times),
	Sum / length(Times).

adding_agents_tc_test_() ->
	{foreach,
	fun() ->
		{ok, File} = file:open(?MODULE_STRING ++ "-profile.txt", [append]),
		{ok, AM} = agent_manager:start([node()]),
		File
	end,
	fun(File) ->
		file:close(File),
		agent_manager:stop()
	end,
	[fun(File) -> Name = "agents with same length skills", {timeout, 60, {Name, fun() ->
		Agents = [#agent{
			login = integer_to_list(X),
			skills = [X rem 5]
		} || X <- lists:seq(1, 1000)],
		Times = [begin
			{T, _} = timer:tc(agent_manager, start_agent, [A]),
			T
		end || A <- Agents],
		[case agent_manager:query_agent(A#agent.login) of
			{true, Pid} ->
				exit(Pid, kill);
			false ->
				ok
		end || A <- Agents],
		lager:info("Average for ~s:  ~f", [Name, avg(Times)]),
		?assert(true),
		io:format(File, "~p	~s:~s(~s)	~f~n", [os:timestamp(), ?MODULE, adding_agents_tc_test, Name, avg(Times)])
	end}} end,
	fun(File) -> Name = "agents with variable length skills", {timeout, 60, {Name, fun() ->
		Agents = [#agent{
			login = integer_to_list(X),
			skills = [S || S <- lists:seq(0, X rem 10)]
		} || X <- lists:seq(1, 1000)],
		Times = [begin
			{T, _} = timer:tc(agent_manager, start_agent, [A]),
			T
		end || A <- Agents],
		[case agent_manager:query_agent(A#agent.login) of
			{true, Pid} ->
				exit(Pid, kill);
			false ->
				ok
		end || A <- Agents],
		lager:info("Average:  ~f", [avg(Times)]),
		io:format(File, "~p	~s:~s(~s)	~f~n", [os:timestamp(), ?MODULE, adding_agent_tc_test, Name, avg(Times)]),
		?assert(true)
	end}} end,
	fun(File) -> Name = "agent skill list shoved in the middle", {timeout, 60, {Name, fun() ->
		LowAgents = [#agent{
			login = integer_to_list(X),
			skills = []
		} || X <- lists:seq(1, 499)],
		HighAgents = [#agent{
			login = integer_to_list(X),
			skills = [english, german]
		} || X <- lists:seq(501, 1000)],
		AllAgents = LowAgents ++ HighAgents,
		[agent_manager:start_agent(A) || A <- AllAgents],
		Times = [begin
			{T, {_, Pid}} = timer:tc(agent_manager, start_agent, [#agent{
				login = "500",
				skills = [english]
			}]),
			exit(Pid, kill),
			% give it a moment to clear it
			timer:sleep(10),
			T
		end || _X <- lists:seq(1, 1000)],
		lager:info("Average:  ~f", [avg(Times)]),
		io:format(File, "~p	~s:~s(~s)	~f~n", [os:timestamp(), ?MODULE, adding_agent_tc_test, Name, avg(Times)]),
		?assert(true)
	end}} end]}.

-ifdef(PROFILE).

avg(Times) ->
	Sum = lists:foldl(fun(E, S) -> E + S end, 0, Times),
	Sum / length(Times).

tdiff({InMeg, InSec, InMic}, {GotMeg, GotSec, GotMic}) ->
	In = InMeg * 1000000 + InSec + InMic / 1000000,
	Got = GotMeg * 1000000 + GotSec + GotMic / 1000000,
	Got - In.

adding_agents_test_() ->
	{foreach,
	fun() ->
		{ok, AM} = agent_manager:start([node()]),
		AM
	end,
	fun(_AM) ->
		agent_manager:stop()
	end,
	[fun(_) -> {timeout, 60, {"agents with same length skills", fun() ->
		Agents = [#agent{
			login = integer_to_list(X),
			skills = [X rem 5]
		} || X <- lists:seq(1, 1000)],
		Times = [begin
			Start = os:timestamp(),
			agent_manager:start_agent(A),
			End = os:timestamp(),
			tdiff(Start, End)
		end || A <- Agents],
		[case agent_manager:query_agent(A#agent.login) of
			{true, Pid} ->
				exit(Pid, kill);
			false ->
				ok
		end || A <- Agents],
		lager:info("Average:  ~f", [avg(Times)]),
		?assert(true)
	end}} end,
	fun(_) -> {timeout, 60, {"agents with variable length skills", fun() ->
		Agents = [#agent{
			login = integer_to_list(X),
			skills = [S || S <- lists:seq(0, X rem 10)]
		} || X <- lists:seq(1, 1000)],
		Times = [begin
			Start = os:timestamp(),
			agent_manager:start_agent(A),
			End = os:timestamp(),
			tdiff(Start, End)
		end || A <- Agents],
		[case agent_manager:query_agent(A#agent.login) of
			{true, Pid} ->
				exit(Pid, kill);
			false ->
				ok
		end || A <- Agents],
		lager:info("Average:  ~f", [avg(Times)]),
		?assert(true)
	end}} end]}.


-endif.

-endif.

