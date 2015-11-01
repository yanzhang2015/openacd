-module(ouc_agents).

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("oacd_ouc.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("reach_core/include/agent.hrl").
-include_lib("reach_core/include/call.hrl").

%% API
-export([start/0, start_link/0, stop/0,
	get_online_agents/0, get_transfer_agents/1,
	subscribe_transfer_agents/0,
	unsubscribe_transfer_agents/0,
	subscribe/0, is_subscribed/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-import(cpx_json_util, [l2b/1, b2l/1, nob/1, nol/1]).

-define(SERVER, {local, ?MODULE}).

-record(state, {agent_pids = gb_trees:empty() :: gb_tree()}).

%% API
-spec start() -> {ok, pid()} | ignore | {error, any()}.
start() ->
	gen_server:start(?SERVER, ?MODULE, [], []).

-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
	gen_server:start_link(?SERVER, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
	gen_server:call(?MODULE, stop).

-spec get_transfer_agents(Agent::string()) -> list().
get_transfer_agents(Agent) ->
	gen_server:call(?MODULE, {get_agents_for_transfer, Agent}).

subscribe_transfer_agents() ->
	catch gproc:add_global_property(ouc_transfer_agents_update, subscribe).

unsubscribe_transfer_agents() ->
	catch gproc:unreg({p, g, ouc_transfer_agents_update}).

-spec get_online_agents() -> [#online_agent{}].
get_online_agents() ->
	gen_server:call(?MODULE, get_online_agents).

-spec subscribe() -> ok.
subscribe() ->
	catch gproc:reg({p, g, ouc_agents_update}, true),
	ok.

-spec is_subscribed() -> boolean().
is_subscribed() ->
	try gproc:get_value({p, g, ouc_agents_update}), true
	catch error:badarg -> false end.

%% gen_server callbacks
init([]) ->
	gproc:add_global_property(cpx_agent_change, subscribe),
	gproc:add_global_property(cpx_agent_channel_change, subscribe),
	agent_state_manager:subscribe(ustate),
	{ok, #state{}}.

handle_call({get_agents_for_transfer, Agent}, _From, State) ->
	Agents = [form_agent(AgentState) ||
		{_Pid, AgentState} <- gproc:lookup_values({p, g, cpx_online_agent}),
		AgentState#cpx_agent_state.login =/= Agent],
	{reply, Agents, State};
handle_call(get_online_agents, _From, State) ->
	Agents = get_online_agents_list(),
	{reply, Agents, State};
handle_call(_Msg, _From, State) ->
	{reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(#cpx_agent_login{prop=AgentProp, pid=Pid}, State) ->
	Agent = get_online_agent(AgentProp),
	Username = AgentProp#cpx_agent_prop.login,
	send_agent_update(Username, Agent),
	erlang:monitor(process, Pid),
	AgentPids = gb_trees:insert(Pid, {agent, Username}, State#state.agent_pids),
	{noreply, State#state{agent_pids = AgentPids}};
handle_info(#cpx_agent_state_update{pid=AgentPid}, State) ->
	case find_online_agent_by_pid(AgentPid) of
		{ok, Agent} ->
			Username = Agent#online_agent.username,
			send_agent_update(Username, Agent);
		_ ->
			ignore
	end,
	{noreply, State};
handle_info(#cpx_agent_channel_state_update{agent_pid=AgentPid}, State) ->
	case find_online_agent_by_pid(AgentPid) of
		{ok, Agent} ->
			Username = Agent#online_agent.username,
			send_agent_update(Username, Agent);
		_ ->
			ignore
	end,
	{noreply, State};
handle_info({cpx_agent_ustate_change, _, _, AgentState}, State) ->
	gproc:send({p, g, ouc_transfer_agents_update}, {ouc_transfer_agents_update,
		{form_agent(AgentState), online}}),
	{noreply, State};
handle_info({cpx_agent_offline, _, AgentState}, State) ->
	gproc:send({p, g, ouc_transfer_agents_update}, {ouc_transfer_agents_update,
		{form_agent(AgentState), offline}}),
	{noreply, State};
handle_info({'DOWN', _, process, Pid, _}, State) ->
	AgentPids = State#state.agent_pids,
	NewAgentPids = case gb_trees:lookup(Pid, AgentPids) of
		none ->
			lager:warning("Received 'DOWN' message from unmonitored process", []),
			State#state.agent_pids;
		{value, {agent, Username}} ->
			send_agent_update(Username, offline),
			gb_trees:delete(Pid, AgentPids)
	end,
	{noreply, State#state{agent_pids = NewAgentPids}};
handle_info(_Msg, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

%% Internal functions

-spec get_online_agents_list() -> [#online_agent{}].
get_online_agents_list() ->
	AgentChannels = get_agent_channels(),
	Agents = qlc:e(qlc:q([Prop || {_, _, Prop = #cpx_agent_prop{}} <- gproc:table({g, p})])),

	lists:map(fun(A) ->
		get_online_agent(A, AgentChannels)
	end, Agents).

-spec get_online_agent(#cpx_agent_prop{}) -> #online_agent{}.
get_online_agent(AgentProp) ->
	%% WARN - inefficient must not have to get ALL channels
	AgentChannels = get_agent_channels(),
	get_online_agent(AgentProp, AgentChannels).

%% -------------------------------------------------------------------------------------------
-spec get_online_agent(#cpx_agent_prop{}, [{string(), #active_channel{}}]) -> #online_agent{}.
%% @private
%% @doc Process data regarding an online agent and returns an #online_agent record
%% @end
%% -------------------------------------------------------------------------------------------
get_online_agent(#cpx_agent_prop{login=Username, profile=Profile, skills=Skills, state=State, start_time=StartTime}, AgentChannels) ->
	Channels = proplists:get_all_values(Username, AgentChannels),

	Db = ouc_db:get_db(profiles),
	{ok, Props} = Db:findOne(<<"userProfile">>, [{<<"m_userName">>, Username}]),
	GetProp = fun(Q) -> case ej:get(Q, Props, undefined) of undefined -> undefined; B when is_binary(B) -> b2l(B) end end,

    Node = case agent_manager:query_agent(Username) of
        {true, AgentPid} ->
           NodeName = ouc_utils:get_node(AgentPid),
           NodeName;
        false ->
            <<"none">>
    end,
    %TODO: remove eventual duplicates: node, skills._node
	#online_agent{
		username = Username,
        profile = Profile,
        skills = Skills,
        state = State,
        reach_node = Node,
		active_channels = Channels,
        login_time = StartTime,
		first_name = GetProp({"m_firstName"}),
        last_name = GetProp({"m_lastName"}),
		location = GetProp({"m_location"}),
        job_title = GetProp({"m_jobTitle"}),
		avatar = GetProp({"m_avatar"})}.

-spec find_online_agent_by_pid(pid()) -> {ok, #online_agent{}} | {error, any()}.
find_online_agent_by_pid(Pid) ->
	{gproc, Vals} = gproc:info(Pid, gproc),
	case proplists:get_value({p, g, cpx_agent}, Vals) of
		undefined ->
			{error, no_gproc_entry};
		AgentProp ->
			{ok, get_online_agent(AgentProp)}
	end.

-spec send_agent_update(string(), #online_agent{} | offline) -> {ouc_agents, {string(), #online_agent{} | offline}}.
send_agent_update(Username, Data) ->
	gproc:send({p, g, ouc_agents_update}, {ouc_agents, [{Username, Data}]}).

-spec get_agent_channels() -> [{string(), #active_channel{}}].
get_agent_channels() ->
	qlc:e(qlc:q([{Login, #active_channel{type=Type, state=State, client=Client, callid=CallId, callerid=CallerId, channelid=ChanId, state_changes=StateChanges}} ||
		{_, _, #cpx_agent_channel_prop{login=Login, type=Type, state=State, client=#client{label=Client}, callid=CallId, callerid=CallerId, channelid=ChanId, state_changes=StateChanges}} <- gproc:table({g, p}), State =/= stop])).

form_agent(AgentState) ->
	#cpx_agent_state{
		login = Login,
		profile = Profile,
		firstname = FirstName,
		lastname = LastName,
		ustate = UState
	} = AgentState,
	FormatName = fun(Name) when is_list(Name) andalso length(Name) > 0 ->
			Name;
		(_) ->
			undefined
	end,
	[
		{login, Login},
		{profile, Profile},
		{first_name, FormatName(FirstName)},
		{last_name, FormatName(LastName)},
		{state, UState}
	].

-ifdef(TEST).

t_agent(Login, Profile, Skills, State, StartTime) ->
	Self = self(),
	Pid = spawn_link(fun() ->
		Prop = #cpx_agent_prop{login=Login, profile=Profile, skills=Skills, state=State, start_time=StartTime},
		gproc:add_global_property(cpx_agent, Prop),
		Event = #cpx_agent_login{pid = self(), prop = Prop},
		gproc:send({p, g, cpx_agent_change}, Event),

		Self ! {waiting, self()},
		receive
			{change_state, NewState} ->
				NewProp = #cpx_agent_prop{login=Login, profile=Profile, skills=Skills, state=NewState, start_time=StartTime},
				gproc:set_value({p, g, cpx_agent}, NewProp),
				NewEvent = #cpx_agent_state_update{pid = self(), prop = NewProp},
				gproc:send({p, g, cpx_agent_change}, NewEvent),
				receive _ -> ok end;
			_ -> ok
		end
	end),

	receive
		{waiting, Pid} ->
			Pid
	after 10 ->
		throw(agent_spawn_fail)
	end.

t_agent_chan(AgentPid, Login, Type, State, Client, CallerId) ->
	Self = self(),
	Pid = spawn_link(fun() ->
		Prop = #cpx_agent_channel_prop{login=Login, type=Type, state=State, client=#client{label=Client}, callerid=CallerId},
		gproc:add_global_property(cpx_agent_channel, Prop),
		Event = #cpx_agent_channel_state_update{pid = self(), agent_pid = AgentPid, prop = Prop},
		gproc:send({p, g, cpx_agent_channel_change}, Event),

		Self ! {waiting, self()},
		receive _ -> ok end
	end),

	receive
		{waiting, Pid} ->
			Pid
	after 10 ->
		throw(agent_chan_spawn_fail)
	end.

t_reg_agents_update() ->
	gproc:add_global_property(cpx_agent_change, subscribe),
	gproc:reg({p, g, ouc_agents_update}).

t_reg_agent_channels_update() ->
	gproc:add_global_property(cpx_agent_channel_change, subscribe).

t_assert_receive_agents_update(Agents) ->
	Update = t_receive_agents(),
	?assertEqual(Agents, Update).

t_receive_agents() ->
	receive
		{ouc_agents, Agents} -> Agents
	after
		0 -> undefined
	end.

get_online_agents_test_() ->
	{foreach, fun() ->
		application:start(gproc),
		meck:new(agent_manager),
		meck:new(mongoapi),
		meck:expect(mongoapi, new, fun(N, Db) -> {mongoapi, N, Db} end),

		meck:new(ouc_rstat),
		meck:expect(ouc_rstat, get_agent_rstats, 0, []),

		start()
	end, fun(_) ->
		stop(),

		gproc:send({p, g, cpx_agent}, exit),
		gproc:send({p, g, cpx_agent_channel}, exit),
		meck:unload()
	end, [{"get online agents", fun() ->
		meck:expect(mongoapi, findOne,
			fun(<<"userProfile">>, [{<<"m_userName">>, "agent1"}], _) ->
				{ok, t_samp_profile("agent1", "first1", "last1", "job1", "loc1")};
			   (<<"userProfile">>, [{<<"m_userName">>, "agent2"}], _) ->
				{ok, t_samp_profile("agent2", "first2", "last2", "job2", "loc2")}
			end),
		meck:expect(agent_manager, list, 0, [{"agent1", #agent_cache{time_avail={13,14,14}}},
			{"agent2", #agent_cache{time_avail={13,15,15}}}]),
		Pid = t_agent("agent1", "sales", ['english', {'_brand', "Client 1"}, '_all'], {released, {"rel0", "Lunch", 0}}, {13,14,14}),
		t_agent("agent2", "marketing", ['english', {'_brand', "Client 2"}, {'_queue', "marketing_queue"}], available, {13,15,15}),
		t_agent_chan(Pid, "agent1", voice, ringing, "Client 1", {"Caller1","Caller1"}),

		?assertEqual([
			#online_agent{username="agent1", profile="sales", login_time={13,14,14}, skills=['english', {'_brand', "Client 1"}, '_all'],
						  state={released, {"rel0", "Lunch", 0}}, active_channels=[#active_channel{type=voice, state=ringing, client="Client 1", callerid={"Caller1","Caller1"}}],
						  first_name="first1", last_name="last1", location="loc1", job_title="job1", avatar="https://oacddev.ezuce.com/sipxconfig/rest/avatar/agent1"},
			#online_agent{username="agent2", profile="marketing", login_time={13,15,15}, skills=['english', {'_brand', "Client 2"}, {'_queue', "marketing_queue"}], state=available,
						  first_name="first2", last_name="last2", location="loc2", job_title="job2", avatar="https://oacddev.ezuce.com/sipxconfig/rest/avatar/agent2"}
		], get_online_agents())
	end}, {"no online agents", fun() ->
		meck:expect(agent_manager, list, 0, []),
		?assertEqual([], get_online_agents())
	end}]}.

send_agent_updates_test_() ->
	{foreach, fun() ->
		application:start(gproc),
		meck:new(agent_manager),
		meck:new(mongoapi),
		meck:expect(mongoapi, new, fun(N, Db) -> {mongoapi, N, Db} end),

		meck:expect(mongoapi, findOne,
			fun(<<"userProfile">>, [{<<"m_userName">>, "agent1"}], _) ->
				{ok, t_samp_profile("agent1", "first1", "last1", "job1", "loc1")}
			end),

		meck:new(ouc_rstat),
		meck:expect(ouc_rstat, get_agent_rstats, 0, []),

		start()
	end, fun(_) ->
		stop(),

		gproc:send({p, g, cpx_agent}, exit),
		gproc:send({p, g, cpx_agent_channel}, exit),
		meck:unload()
	end, [{"receive agents update", fun() ->
		meck:expect(agent_manager, list, 0, [{"agent1", #agent_cache{time_avail={13,14,14}}}]),

		?assertEqual([], get_online_agents()),

		t_reg_agents_update(),

		Pid = t_agent("agent1", "sales", ['english', {'_brand', "Client 1"}, '_all'], {released, {"rel0", "Lunch", 0}}, {13,14,14}),
		timer:sleep(110),
		t_assert_receive_agents_update([{"agent1", #online_agent{username="agent1", profile="sales", login_time={13,14,14}, skills=['english', {'_brand', "Client 1"}, '_all'],
			state={released, {"rel0", "Lunch", 0}}, active_channels=[], first_name="first1", last_name="last1", location="loc1", job_title="job1", avatar="https://oacddev.ezuce.com/sipxconfig/rest/avatar/agent1"}}]),

		Pid ! kill,
		timer:sleep(110),
		t_assert_receive_agents_update([{"agent1", offline}])
	end}, {"receive agent state updates", fun() ->
		meck:expect(agent_manager, list, 0, [{"agent1", #agent_cache{time_avail={13,14,14}}}]),

		t_reg_agents_update(),
		t_reg_agent_channels_update(),

		Pid = t_agent("agent1", "sales", ['english', {'_brand', "Client 1"}, '_all'], {released, {"rel0", "Lunch", 0}}, {13,14,14}),
		timer:sleep(110),
		t_assert_receive_agents_update([{"agent1", #online_agent{username="agent1", profile="sales", login_time={13,14,14}, skills=['english', {'_brand', "Client 1"}, '_all'],
			state={released, {"rel0", "Lunch", 0}}, active_channels=[], first_name="first1", last_name="last1", location="loc1", job_title="job1", avatar="https://oacddev.ezuce.com/sipxconfig/rest/avatar/agent1"}}]),

		Pid ! {change_state, available},
		timer:sleep(110),
		t_assert_receive_agents_update([{"agent1", #online_agent{username="agent1", profile="sales", login_time={13,14,14}, skills=['english', {'_brand', "Client 1"}, '_all'],
			state=available, active_channels=[], first_name="first1", last_name="last1", location="loc1", job_title="job1", avatar="https://oacddev.ezuce.com/sipxconfig/rest/avatar/agent1"}}])
	end}, {"receive channel state updates", fun() ->
		meck:expect(agent_manager, list, 0, [{"agent1", #agent_cache{time_avail={13,14,14}}}]),

		t_reg_agents_update(),
		t_reg_agent_channels_update(),

		Pid = t_agent("agent1", "sales", ['english', {'_brand', "Client 1"}, '_all'], available, {13,14,14}),
		timer:sleep(110),
		t_assert_receive_agents_update([{"agent1", #online_agent{username="agent1", profile="sales", login_time={13,14,14}, skills=['english', {'_brand', "Client 1"}, '_all'],
			state=available, active_channels=[], first_name="first1", last_name="last1", location="loc1", job_title="job1", avatar="https://oacddev.ezuce.com/sipxconfig/rest/avatar/agent1"}}]),

		t_agent_chan(Pid, "agent1", voice, ringing, "Client 1", {"Caller1","Caller1"}),
		timer:sleep(110),
		t_assert_receive_agents_update([{"agent1", #online_agent{username="agent1", profile="sales", login_time={13,14,14}, skills=['english', {'_brand', "Client 1"}, '_all'],
			state=available, active_channels=[#active_channel{type=voice, state=ringing, client="Client 1", callerid={"Caller1","Caller1"}}], first_name="first1", last_name="last1", location="loc1", job_title="job1", avatar="https://oacddev.ezuce.com/sipxconfig/rest/avatar/agent1"}}])
	end}]}.

t_samp_profile(Username, FirstName, LastName, JobTitle, Location) ->
	Avatar = "https://oacddev.ezuce.com/sipxconfig/rest/avatar/" ++ Username,
	[{<<"_id">>,list_to_binary(Username)},
	 {<<"_class">>,
	  <<"org.sipfoundry.commons.userdb.profile.UserProfile">>},
	 {<<"m_userName">>,list_to_binary(Username)},
	 {<<"m_firstName">>,list_to_binary(FirstName)},
	 {<<"m_lastName">>,list_to_binary(LastName)},
	 {<<"m_jobTitle">>,list_to_binary(JobTitle)},
	 {<<"m_jobDept">>,<<"Dept">>},
	 {<<"m_companyName">>,<<"Company">>},
	 {<<"m_assistantName">>,<<"Asst.">>},
	 {<<"m_location">>,list_to_binary(Location)},
	 {<<"m_homeAddress">>,
	  [{<<"m_street">>,<<"home street">>},
	   {<<"m_city">>,<<"home city">>},
	   {<<"m_country">>,<<"home ctry">>},
	   {<<"m_state">>,<<"home state">>},
	   {<<"m_zip">>,<<"home zip">>}]},
	 {<<"m_officeAddress">>,
	  [{<<"m_street">>,<<"ofc street">>},
	   {<<"m_city">>,<<"ofc city">>},
	   {<<"m_country">>,<<"ofc country">>},
	   {<<"m_state">>,<<"ofc state">>},
	   {<<"m_zip">>,<<"ofc zip">>},
	   {<<"m_officeDesignation">>,<<"ofc desig">>}]},
	 {<<"m_cellPhoneNumber">>,<<"cell">>},
	 {<<"m_homePhoneNumber">>,<<"homephn">>},
	 {<<"m_assistantPhoneNumber">>,<<"asstphn">>},
	 {<<"m_faxNumber">>,<<"fax">>},
	 {<<"m_didNumber">>,<<"did">>},
	 {<<"m_imId">>, Username},
	 {<<"m_alternateImId">>,<<"altim">>},
	 {<<"m_emailAddress">>,<<"agent@oacddev.ezuce.com">>},
	 {<<"m_alternateEmailAddress">>,<<"altagent@a.com">>},
	 {<<"m_useBranchAddress">>,false},
	 {<<"m_manager">>,<<"manager">>},
	 {<<"m_salutation">>,<<"None">>},
	 {<<"m_employeeId">>,<<"emp">>},
	 {<<"m_twiterName">>,<<"twitter">>},
	 {<<"m_linkedinName">>,<<"linkedin">>},
	 {<<"m_facebookName">>,<<"fb">>},
	 {<<"m_xingName">>,<<"xing">>},
	 {<<"m_avatar">>,list_to_binary(Avatar)},
	 {<<"m_useExtAvatar">>,false}].

-endif.
