-module(ouc_gen_media_listener).
-behaviour(gen_event).

%% API
-export([
	register/0,
	get_cdr_mods/0
]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
	handle_info/2, code_change/3, terminate/2]).

-include_lib("reach_core/include/agent.hrl").
-include_lib("reach_core/include/call.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SERVER, ?MODULE).

-record(state, {
	id,
	client,
	last_agent,
	last_queue,
	state_changes = [],
	mon :: reference()
}).

%% API

register() ->
	cpx_hooks:set_hook(?MODULE, get_cdr_mods, ?MODULE, get_cdr_mods, [], 100).
get_cdr_mods() ->
	{ok, [?MODULE]}.

%% gen_event callbacks

init([Call]) ->
	CallId = Call#call.id,
	Client = case Call#call.client of
		#client{label=L} -> L;
		_ -> undefined
	end,

	Mon = erlang:monitor(process, Call#call.source),

	{ok, #state{id = CallId, client = Client, mon = Mon}}.

handle_event({Tran, Call=#call{id = CallId}, Time, Data},
		State=#state{id = CallId}) ->
	handle_tran(Tran, Time, Data, Call, State);
handle_event(_Event, State) ->
	{ok, State}.

handle_call(_Request, State) ->
	Reply = ok,
	{ok, Reply, State}.
handle_info({'DOWN', Mon, process, _Pid, _Reason}, State=#state{mon = Mon}) ->
	Time = ouc_time:now(),
	State1 = log_state_change(stop, Time, State, []),
	mn_log_call(State1, Time),
	remove_handler;
handle_info(_Info, State) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal functions
handle_tran(T=inivr, Time, _Data, _Call, State) ->
	{ok, log_state_change(T, Time, State, [])};

handle_tran(T=inqueue, Time, Data, _Call, State) ->
	State1 = log_state_change(T, Time, State, [{queue, Data}]),

	State2 = case get_last_event(State) of
		{ok, inivr} ->
			State1;
		{ok, _} ->
			mn_log_call(State1, Time),
			log_state_change(T, Time, State1#state{state_changes=[]}, [{queue, Data}]);
		_ ->
			State1
	end,
	{ok, State2#state{last_queue = Data}};

handle_tran(T=ringing, Time, Data, _Call, State) ->
	AgentLogin = proplists:get_value(agent_login, Data),
	{ok, log_state_change(T, Time, State, [{agent, AgentLogin}])};

handle_tran(T=oncall, Time, Data, _Call, State) ->
	%% might not exactly be the same state from on call was
	%% initiated. should change cdr call to pass agent rec instead
	% [{_, AgentLogin}, _] = Data,
	AgentLogin = Data,
	Agent = get_agent(AgentLogin),

	State1 = log_state_change(T, Time, State, [{agent, Agent}]),
	{ok, State1#state{last_agent=Agent}};

% handle_tran(T=hangup, Time, _Data, _Call, State) ->
% 	State1 = log_state_change(T, Time, State, []),
% 	mn_log_call(State1, Time),
% 	remove_handler;

handle_tran(T=wrapup, Time, _Data, _Call, State) ->
	case get_last_event(State) of
		{ok, inqueue} ->
			{ok, State};
		_ ->
			State1 = log_state_change(T, Time, State, []),
			mn_log_call(State1, Time),
			remove_handler
	end;

handle_tran(_T, _Time, _Data, _Call, State) ->
	{ok, State}.

log_state_change(T, Time, State, Ps) ->
	SCs = State#state.state_changes,
	State#state{state_changes=[{T, Time, Ps}|SCs]}.

mn_log_call(State, Ts) ->
	%% @todo -- this isn't enough when transfers come in

	% simplifying. we're losing data here...
	CallH = lists:reverse([{Zt, Time} ||
		{Zt, Time, _Data} <- State#state.state_changes]),

	Det0 = [{client, State#state.client},
		{queue, State#state.last_queue}],
	Det1 = case State#state.last_agent of
		#agent{login=L, profile=P} ->
			[{agent, L}, {profile, P}|Det0];
		_ ->
			Det0
	end,

	ouc_rstat:log_call(CallH, Det1, Ts).

get_agent(Login) ->
	case agent_manager:query_agent(Login) of
		{true, Pid} ->
			agent:dump_state(Pid);
		_ ->
			undefined
	end.

get_last_event(State) ->
	SCs = State#state.state_changes,
	case SCs of
		[] ->
			undefined;
		[{Last, _, _}|_] ->
			{ok, Last}
	end.

%% Tests

-ifdef(TEST).

mn_log_test_() ->
	{setup, fun() ->
		%% silence sasl
		error_logger:delete_report_handler(error_logger_tty_h),

		meck:new(ouc_rstat),
		meck:expect(ouc_rstat, log_call, 3, ok),

		application:start(gproc),
		cpx_hooks:start_link(),

		APid = cpx_dummy_pid:start_link(),

		meck:new(agent_manager),
		meck:expect(agent_manager, query_agent,
			fun("suzie") -> {true, APid} end),

		meck:new(agent),
		meck:expect(agent, dump_state, 1,
			#agent{login="suzie", profile="Default"})
	end, fun(_) ->
		meck:unload()
	end, [fun() ->
		CallPid = cpx_dummy_pid:start_link(),
		Call = #call{id="testcall", client=#client{label="client1"},
			source=CallPid},

		AgentPid = list_to_pid("<0.0.0>"),

		{ok, EvMgr} = gen_event:start(),
		gen_event:add_sup_handler(EvMgr, ?MODULE, [Call]),

		gen_event:notify(EvMgr, {inivr,	Call, dtm({05, 00, 03}), []}),
		gen_event:notify(EvMgr, {inqueue, Call, dtm({05, 00, 04}), "queue1"}),
		gen_event:notify(EvMgr, {ringing, Call, dtm({05, 00, 08}),
			[{agent_name, "suzie"}, {agent_pid, AgentPid}]}),
		gen_event:notify(EvMgr, {oncall, Call, dtm({05, 00, 10}),
			"suzie"}),
		gen_event:sync_notify(EvMgr, {hangup, Call, dtm({05, 00, 30}), []}),
		gen_event:stop(EvMgr),

		?assert(meck:called(ouc_rstat, log_call, [
			[
				{inivr,	dtm({05, 00, 03})},
				{inqueue, dtm({05, 00, 04})},
				{ringing, dtm({05, 00, 08})},
				{oncall, dtm({05, 00, 10})},
				{hangup, dtm({05, 00, 30})}
			], [
				{agent, "suzie"},
				{profile, "Default"},
				{client, "client1"},
				{queue, "queue1"}
			], dtm({05, 00, 30})]))

	end]}.

dtm(Tm) ->
	ouc_time:datetime_to_unixts({{2012, 01, 01}, Tm}).

-endif.
