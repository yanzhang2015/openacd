-module(ouc_agent_listener).
-behaviour(gen_server).

-include_lib("reach_core/include/call.hrl").
-include_lib("reach_core/include/agent.hrl").

-export([
	start_link/1,
	stop/1
]).

%% debug api
% -export([
% 	get_agent_cstate/1
% ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SERVER, ?MODULE).
-define(DICT, orddict). %% data is small anyway
-define(GPROC_NAME(AgentId), {n, g, {?MODULE, AgentId}}).

-record(state, {
	id,
	login,
	profile,
	clients,
	pid,
	amon,
	c_pids = ?DICT:new(),
	ringing_count = 0,
	oncall_count = 0,
	wrapup_count = 0,

	ustate :: released | idle | ringing | oncall | wrapup,
	rstate :: available | released,
	oucstate,

	last_change,

	delayed_register :: undefined | reference()
}).

%% debug api
% get_agent_cstate(Id) ->
% 	gen_event:call(cpx_agent_event, {?MODULE, Id},
% 		get_agent_cstate).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link([Agent, Now]) ->
	gen_server:start_link(?MODULE, [Agent, Now], []).

%%--------------------------------------------------------------------
%% Function: stop(Pid) -> ok | error
%% Description: Stops the server
%%--------------------------------------------------------------------
stop(Pid) ->
	case catch gen_server:call(Pid, stop, 1000) of
		ok ->
			ok;
		_Err ->
			error
	end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------

init([Agent, Now]) ->
	#agent{
		id = Id,
		login = Login,
		profile = Profile,
		source = Pid,
		release_data = RelData,
		skills = Skills
	} = Agent,
	RState = rel_to_rstate(RelData),
	UState = case RState of
		released -> released;
		_ -> idle
	end,
	AMon = monitor(process, Pid),
	Clients = [Cl || {'_brand', Cl} <- Skills],

	St = #state{
		id = Id,
		login = Login,
		profile = Profile,
		clients = Clients,

		pid = Pid,
		amon = AMon,

		rstate = RState,
		ustate = UState,

		last_change = Now
	},

	St2 = register_name(Id, St),

	ouc_agent_profiles:update_profile_counts(Profile, undefined, UState),
	ouc_rstat:subscribe_prelog(),

	{ok, St2}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(get_agent_cstate, _From, #state{ustate=UState, last_change=LastChange} = St) ->
	Reply = {ok, UState, LastChange},
	{reply, Reply, St};

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(_Request, _From, St) ->
	Reply = ok,
	{reply, Reply, St}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({change_release_state, AgentId, Rel, Ts}, St=#state{id=AgentId}) ->
	RState = case Rel of
		available -> available;
		_ -> released
	end,
	St1 = handle_arel(RState, Ts, St),
	{noreply, St1};
handle_info({channel_init, #agent{id=AgentId}, CPid, CStateName, _CStateData, Now}, St=#state{id=AgentId}) ->
	erlang:monitor(process, CPid),
	CPids0 = St#state.c_pids,
	St1 = St#state{c_pids=?DICT:store(CPid, CStateName, CPids0)},

	St2 = handle_chan(new, CStateName, Now, St1),
	{noreply, St2};
handle_info({channel_change, _Agent, CPid, CStateName, _CStateData, Now}, St) ->
	case ?DICT:find(CPid, St#state.c_pids) of
		{ok, OldCStateName} ->
			%% would've been better if find & store were one step
			CPids1 = ?DICT:store(CPid, CStateName, St#state.c_pids),
			St1 = St#state{c_pids=CPids1},
			St2 = handle_chan(OldCStateName, CStateName, Now, St1),
			{noreply, St2};
		_ ->
			{noreply, St}
	end;
handle_info({ouc_rstat_prelog, Now, Ref}, St) ->
	log_state_change(St, St, Now),
	ouc_rstat:notify_logged(Ref),
	{noreply, St#state{last_change=Now}};
handle_info({'DOWN', AMon, process, _, _}, #state{amon=AMon} = St) ->
	%% todo log

	Now = ouc_time:now(),
	St2 = handle_arel(down, Now, St),

	{stop, normal, St2};
handle_info({'DOWN', _, process, CPid, _}, St) ->
	CPids = St#state.c_pids,
	Now = ouc_time:now(),

	St2 = case ?DICT:find(CPid, St#state.c_pids) of
		{ok, OldCStateName} ->
			CPids = St#state.c_pids,
			St1 = St#state{c_pids=?DICT:erase(CPid, CPids)},
			handle_chan(OldCStateName, down, Now, St1);
		_ ->
			St
	end,
	{noreply, St2};

handle_info({gproc, unreg, Ref, ?GPROC_NAME(Id)}, #state{delayed_register=Ref, id=Id} = St) ->
	lager:info("Delayed register of ouc_agent_listener for ~p", [Id]),
	gproc:reg(?GPROC_NAME(Id)),
	{noreply, St#state{delayed_register = undefined}};

handle_info(check_delayed_register, #state{delayed_register=Ref, id=Id} = St) when Ref =/= undefined ->
	Name = ?GPROC_NAME(Id),
	case gproc:where(Name) of
		Other when is_pid(Other) ->
			lager:warning("Killing old ouc_agent_listener at ~p for ~p", [Other, Id]),
			erlang:exit(Other, kill);
		_ ->
			ok
	end,
	{noreply, St};

handle_info(_Info, St) ->
	{noreply, St}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal functions

register_name(Id, St) ->
	Name = ?GPROC_NAME(Id),
	case gproc:reg_or_locate(Name) of
		{Pid, _} when Pid =:= self() ->
			lager:info("Registered as ouc_agent_listener for ~p", [Id]),
			St;
		{Other, _} ->
			Ref = gproc:monitor(Name),

			lager:info("Attempting to stop old ouc_agent_listener at ~p for ~p", [Other, Id]),
			ouc_agent_listener:stop(Other),

			erlang:send_after(3000, self(), check_delayed_register),
			St#state{delayed_register = Ref}
	end.

handle_arel(available, Now, #state{ustate = released}=St) ->
	St1 = St#state{ustate = idle, rstate = available, last_change = Now},
	log_state_change(St, St1, Now);
handle_arel(released, Now, #state{ustate=idle}=St) ->
	St1 = St#state{ustate = released, rstate = released, last_change=Now},
	log_state_change(St, St1, Now);
handle_arel(released, _Now, St) ->
	St#state{rstate = released};
handle_arel(down, Now, St) ->
	log_state_change(St, St, Now);
handle_arel(_, _, St) ->
	St.

handle_chan(OldCStateName, CStateName, Now, St) ->
	St1 = update_counts(OldCStateName, CStateName, St),
	update_state(CStateName, Now, St1).

update_counts(StNm, StNm, St) -> St;
update_counts(Old, New, St) ->
	St1 = incr_count(Old, St, -1),
	St2 = incr_count(New, St1, 1),

	St2.

incr_count(StNm, St, Incr) ->
	At = case StNm of
		ringing -> #state.ringing_count;
		oncall -> #state.oncall_count;
		wrapup -> #state.wrapup_count;
		_ -> -1
	end,

	case (At > 0) of
		true ->
			V0 = element(At, St),
			setelement(At, St, V0 + Incr);
		_ ->
			St
	end.

update_state(ringing, Now, St)->
	case St#state.ustate of
		oncall ->
			St;
		ringing ->
			St;
		_ ->
			St1 = St#state{ustate = ringing, last_change = Now},
			log_state_change(St, St1, Now)
	end;
update_state(oncall, Now, St) ->
	St1 = St#state{ustate = oncall},

	case St#state.ustate of
		oncall ->
			St1;
		_ ->
			St2 = St1#state{last_change=Now},
			log_state_change(St, St2, Now)
	end;
update_state(wrapup, Now, St) ->
	UState0 = St#state.ustate,
	UState = case {St#state.oncall_count, St#state.ringing_count} of
		{0, 0} -> wrapup;
		{0, _} -> ringing;
		_ -> oncall
	end,
	St1 = St#state{ustate = UState},

	case UState of
		UState0 ->
			St1;
		_ ->
			St2 = St1#state{last_change = Now},
			log_state_change(St, St2, Now)
	end;
update_state(down, Now, St) ->
	UState0 = St#state.ustate,
	UState = case {St#state.oncall_count, St#state.ringing_count,
			St#state.wrapup_count, St#state.rstate} of
		{0, 0, 0, available} -> idle;
		{0, 0, 0, _} -> released;
		{0, 0, _, _} -> wrapup;
		{0, _, _, _} -> ringing;
		_ -> oncall
	end,
	St1 = St#state{ustate = UState},

	case UState of
		UState0 ->
			St1;
		_ ->
			St2 = St1#state{last_change = Now},
			log_state_change(St, St2, Now)
	end;
update_state(_, _Now, St) ->
	St.

rel_to_rstate(undefined) -> available;
rel_to_rstate(_) -> released.

log_state_change(St, St1, Now) ->
	Login = St#state.login,
	Profile = St#state.profile,
	Clients = St#state.clients,
	LastChange = St#state.last_change,

	LastUState = St#state.ustate,

	Details = [{agent, Login}, {profile, Profile}, {clients, Clients}],

	ouc_rstat:log_ustate(LastUState, LastChange, Now, Details, Now),
	ouc_agent_profiles:update_profile_counts(Profile, LastUState,
		St1#state.ustate),

	St1.

%% Tests

-ifdef(TEST).

-define(M, ?MODULE).

log_test_() ->
	{setup, fun() ->
		error_logger:delete_report_handler(sasl_report_tty_h),
		error_logger:delete_report_handler(error_logger_tty_h),

		meck:new(ouc_rstat),
		meck:expect(ouc_rstat, get_conf, 0, t_conf()),
		meck:expect(ouc_rstat, subscribe_prelog, 0, ok),
		meck:expect(ouc_rstat, log_ustate, 5, ok),

		meck:new(ouc_rstat_store),
		meck:expect(ouc_rstat_store, log_agent_cstate, 7, ok)

		%% hmm. might be better to call ouc_rstat:start to do this if possible
		% cpx_global:new(),
		% cpx_global:append(cpx_agent_listeners, ouc_agent_listener)
	end, fun(_) ->
		meck:unload()
	end,
	{foreach, fun() ->
		application:start(gproc),
		cpx_agent_event:start(),
		gen_event:add_handler(cpx_agent_event, ouc_agent_event_handler, [])
	end, fun(_) ->
		application:stop(gproc),
		cpx_agent_event:stop()
	end, [
	{"online released then stop", fun() ->
		APid = t_start_agent(),
		t_assert_ustate(released),

		ouc_time:set_now(1010),
		cpx_dummy_pid:stop(APid),
		t_assert_undef_listener()

		% ?assert(t_logged(released, 1000, 1010))
	end},
	{"online released, avail, then stop", fun() ->
		APid = t_start_agent(),

		cpx_agent_event:change_release_state("agent", available, 1015),
		t_assert_ustate(idle),

		% ?assert(t_logged(released, 1000, 1015)),

		ouc_time:set_now(1025),
		cpx_dummy_pid:stop(APid)

		% ?assert(t_logged(idle, 1015, 1025))
	end},
	{"online released, avail, released then stop", fun() ->
		APid = t_start_agent(),

		cpx_agent_event:change_release_state("agent", available, 1015),
		t_assert_ustate(idle),
		% ?assert(t_logged(released, 1000, 1015)),

		t_go_released(1018),
		t_assert_ustate(released),
		% ?assert(t_logged(idle, 1015, 1018)),

		ouc_time:set_now(1025),
		cpx_dummy_pid:stop(APid)

		% ?assert(t_logged(released, 1018, 1025))
	end},
	{"single call cycle - prering start", fun() ->
		APid = t_start_agent(),
		t_assert_ustate(released),
		Agent = t_agent(APid),
		CPid = cpx_dummy_pid:start(),

		cpx_agent_event:change_release_state("agent", available, 1015),
		t_assert_ustate(idle),

		Call = #call{id="1234", source=cpx_dummy_pid:start_link()},
		t_start_agent_channel(APid, CPid, Call, prering, 1018),
		% prering is disregarded

		cpx_agent_event:change_agent_channel(Agent, CPid, ringing, Call, 1018),
		t_assert_ustate(ringing),
		% ?assert(t_logged(idle, 1015, 1018)),

		cpx_agent_event:change_agent_channel(Agent, CPid, oncall, Call, 1020),
		t_assert_ustate(oncall),
		% ?assert(t_logged(ringing, 1018, 1020)),

		cpx_agent_event:change_agent_channel(Agent, CPid, wrapup, Call, 1023),
		t_assert_ustate(wrapup),
		% ?assert(t_logged(oncall, 1020, 1023)),

		ouc_time:set_now(1029),
		cpx_dummy_pid:stop(CPid),
		t_assert_ustate(idle),
		% ?assert(t_logged(wrapup, 1023, 1029)),

		ouc_time:set_now(1032),
		cpx_dummy_pid:stop(APid)

		% ?assert(t_logged(idle, 1029, 1032))
	end},
	{"chan start at ringing", fun() ->
		APid = t_start_agent(),
		CPid = cpx_dummy_pid:start(),

		cpx_agent_event:change_release_state("agent", available, 1015),

		Call = #call{id="1234", source=cpx_dummy_pid:start_link()},
		t_start_agent_channel(APid, CPid, Call, ringing, 1018),
		t_assert_ustate(ringing)
		% ?assert(t_logged(idle, 1015, 1018))
	end},
	{"goes available while oncall", fun() ->
		APid = t_start_agent(),
		CPid = cpx_dummy_pid:start(),

		cpx_agent_event:change_release_state("agent", available, 1015),
		t_assert_ustate(idle),

		Call = #call{id="1234", source=cpx_dummy_pid:start_link()},
		t_start_agent_channel(APid, CPid, Call, ringing, 1018),
		t_assert_ustate(ringing),

		cpx_agent_event:change_release_state("agent", available, 1020),
		t_assert_ustate(ringing)
	end},
	{"goes released while oncall", fun() ->
		APid = t_start_agent(),
		Agent = t_agent(APid),
		CPid = cpx_dummy_pid:start(),

		cpx_agent_event:change_release_state("agent", available, 1015),
		t_assert_ustate(idle),

		Call = #call{id="1234", source=cpx_dummy_pid:start_link()},
		t_start_agent_channel(APid, CPid, Call, prering, 1016),
		t_assert_ustate(idle),

		cpx_agent_event:change_agent_channel(Agent, CPid, ringing, Call, 1018),
		t_assert_ustate(ringing),
		% ?assert(t_logged(idle, 1015, 1018)),

		cpx_agent_event:change_agent_channel(Agent, CPid, oncall, Call, 1020),
		t_assert_ustate(oncall),
		% ?assert(t_logged(ringing, 1018, 1020)),

		t_go_released(1021),
		t_assert_ustate(oncall),

		cpx_agent_event:change_agent_channel(Agent, CPid, wrapup, Call, 1023),
		t_assert_ustate(wrapup),
		% ?assert(t_logged(oncall, 1020, 1023)),

		ouc_time:set_now(1029),
		cpx_dummy_pid:stop(CPid),
		t_assert_ustate(released),
		% ?assert(t_logged(wrapup, 1023, 1029)),

		ouc_time:set_now(1033),
		cpx_dummy_pid:stop(APid)
		% ?assert(t_logged(released, 1029, 1033))
	end},
	{"double ring", fun() ->
		APid = t_start_agent(),
		Agent = t_agent(APid),
		CPid = cpx_dummy_pid:start(),

		cpx_agent_event:change_release_state("agent", available, 1015),
		Call = #call{id="1234", source=cpx_dummy_pid:start_link()},
		t_start_agent_channel(APid, CPid, Call, ringing, 1016),
		cpx_agent_event:change_agent_channel(Agent, CPid, ringing, Call, 1020),

		ouc_time:set_now(1022),
		cpx_dummy_pid:stop(CPid)
		% ?assert(t_logged(ringing, 1016, 1022))
	end},
	{"multi - ring A, oncall A, ring B, wrapup A, oncall B", fun() ->
		APid = t_start_agent(),
		Agent = t_agent(APid),

		cpx_agent_event:change_release_state("agent", available, 1015),

		CPidA = cpx_dummy_pid:start(),
		CallA = #call{id="1234", source=cpx_dummy_pid:start_link()},
		t_start_agent_channel(APid, CPidA, ringing, CallA, 1016),
		cpx_agent_event:change_agent_channel(Agent, CPidA, oncall, CallA, 1018),

		CPidB = cpx_dummy_pid:start(),
		CallB = #call{id="2345", source=cpx_dummy_pid:start_link()},

		t_start_agent_channel(APid, CPidB, CallB, ringing, 1020),

		cpx_agent_event:change_agent_channel(Agent, CPidA, wrapup, CallA, 1022),
		% ?assert(t_logged(oncall, 1018, 1022)),

		cpx_agent_event:change_agent_channel(Agent, CPidB, oncall, CallB, 1025),
		% ?assert(t_logged(ringing, 1022, 1025)),

		cpx_agent_event:change_agent_channel(Agent, CPidB, wrapup, CallB, 1029)
		% ?assert(t_logged(oncall, 1025, 1029))
	end},
	{"multi - oncall A, ring B, oncall B, wrapup A, wrapup B, end B, end A", fun() ->
		APid = t_start_agent(),
		Agent = t_agent(APid),

		cpx_agent_event:change_release_state("agent", available, 1015),

		CPidA = cpx_dummy_pid:start(),
		CallA = #call{id="1234", source=cpx_dummy_pid:start_link()},
		t_start_agent_channel(APid, CPidA, CallA, ringing, 1016),

		% oncall A
		cpx_agent_event:change_agent_channel(Agent, CPidA, oncall, CallA, 1018),

		CPidB = cpx_dummy_pid:start(),
		CallB = #call{id="2345", source=cpx_dummy_pid:start_link()},

		% ring B
		t_start_agent_channel(APid, CPidB, CallB, ringing, 1020),

		% oncall B
		cpx_agent_event:change_agent_channel(Agent, CPidB, oncall, CallB, 1025),

		% wrapup A
		cpx_agent_event:change_agent_channel(Agent, CPidA, wrapup, CallA, 1027),

		% wrapup B
		cpx_agent_event:change_agent_channel(Agent, CPidB, wrapup, CallB, 1029),
		% ?assert(t_logged(oncall, 1018, 1029)),

		ouc_time:set_now(1032),
		cpx_dummy_pid:stop(CPidB),

		ouc_time:set_now(1033),
		cpx_dummy_pid:stop(CPidA)

		% ?assert(t_logged(wrapup, 1029, 1033))
	end},
	{"double ring, multi chan", fun() ->
		APid = t_start_agent(),
		Agent = t_agent(APid),

		cpx_agent_event:change_release_state("agent", available, 1015),

		CPidA = cpx_dummy_pid:start(),
		CallA = #call{id="1234", source=cpx_dummy_pid:start_link()},
		t_start_agent_channel(APid, CPidA, CallA, ringing, 1016),
		cpx_agent_event:change_agent_channel(Agent, CPidA, ringing, CallA, 1025),

		CPidB = cpx_dummy_pid:start(),
		CallB = #call{id="2345", source=cpx_dummy_pid:start_link()},
		t_start_agent_channel(APid, CPidB, CallB, ringing, 1022),
		cpx_agent_event:change_agent_channel(Agent, CPidB, oncall, CallB, 1025),

		cpx_agent_event:change_agent_channel(Agent, CPidA, oncall, CallA, 1026),

		cpx_agent_event:change_agent_channel(Agent, CPidA, wrapup, CallA, 1027),
		cpx_agent_event:change_agent_channel(Agent, CPidB, wrapup, CallB, 1028),

		ouc_time:set_now(1030),
		cpx_dummy_pid:stop(APid)
		% ?assert(t_logged(wrapup, 1028, 1030))
	end}
	% {"get_agent_cstate", fun() ->
	% 	t_start_agent(),

	% 	cpx_agent_event:change_release_state("agent", available, 1015),
	% 	?assertEqual({ok, idle, 1015}, ?M:get_agent_cstate("agent"))
	% end},
	% {"log tick", fun() ->
	% 	t_start_agent(),

	% 	?assertEqual({ok, released, 1000}, ?M:get_agent_cstate("agent")),
	% 	gen_event:sync_notify(cpx_agent_event, {ouc_rstat_tick, t_conf(), 1050}), %% todo put in api
	% 	?assertEqual({ok, released, 1050}, ?M:get_agent_cstate("agent"))
	% end}
	]}}.

%% to add:
%% oncall becomes ring

t_start_agent() ->
	APid = cpx_dummy_pid:start_link(),
	Agent = t_agent(APid),
	ouc_time:set_now(1000),
	cpx_agent_event:agent_init(Agent),
	APid.

t_start_agent_channel(APid, CPid, Call, CState, Now) ->
	cpx_agent_event:agent_channel_init(t_agent(APid), CPid, CState, Call, Now).

t_agent(APid) ->
	#agent{
		id = "agent",
		login = "agent",
		source = APid,
		skills = [{'_brand', "client1"}, {'_brand', "client2"}],
		release_data = {"default", default, -1}}.

t_go_released(Ts) ->
	cpx_agent_event:change_release_state("agent", {released, {"default", default, -1}}, Ts).

t_conf() ->
	{ok, Conf} = ouc_rstat_conf:new([{rules, [{dynamic, last_30m, {minute, 30}, 5}]}]),
	Conf.

% t_logged(UState, F, T) ->
% 	timer:sleep(20),
% 	EvPid = whereis(cpx_agent_event),
% 	Called = meck:called(ouc_rstat_store, log_agent_cstate,
% 		[ouc_rstat_store_mnesia:get_store(), t_conf(), UState, F, T,
% 		[[{agent, "agent"}],
% 			[{profile, "Default"}],
% 			[{client, "client1"}],
% 			[{client, "client2"}]], T], EvPid),

% 	% case Called of false -> ?debugFmt("~p", [lists:last(meck:history(ouc_rstat_store))]); _ -> ok end,
% 	Called.

t_assert_ustate(UState) ->
	timer:sleep(100),
	AgentId = "agent",
	Listener = ouc_agent_event_handler:get_agent_listener(AgentId),
	?assertEqual([{Listener, UState}], gproc:lookup_values({p, g, ouc_ustate})),
	?assert(lists:member(Listener, gproc:lookup_pids({p, g, {ouc_profile_state, "Default", UState}}))).

t_assert_undef_listener() ->
	timer:sleep(50),
	AgentLogin = "agent",
	?assertEqual(undefined, ouc_agent_event_handler:get_agent_listener(AgentLogin)).

-endif.
