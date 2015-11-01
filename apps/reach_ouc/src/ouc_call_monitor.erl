-module(ouc_call_monitor).

-behaviour(gen_server).

-compile({parse_transform, do}).

-include_lib("reach_core/include/agent.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("reach_core/include/call.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% api
-export([start_link/1]).
-export([get_handler/1, join_call/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(monitored_call, {
	node :: atom(),
	monitor_callid :: string(),
	callid :: string(),
	channelid :: string(),
	agent :: string(),
	type :: monitor | barge
}).
-record(state, {
	agent_pid :: pid(),
	channel_id :: string(),
	login :: string(),
	connection :: pid(),
	monitored_call = undefined :: undefined | #monitored_call{}
}).

-import(cpx_json_util, [l2b/1, b2l/1, nob/1]).

-define(DEFAULT_DIALSTRING_ARGS, ["origination_caller_id_name='Reach'", "origination_caller_id_number='Reach'"]).

%% api

-spec start_link(AgentPid::pid()) -> {ok, pid()}.
start_link(AgentPid) ->
	gen_server:start_link(?MODULE, [AgentPid], []).

-spec get_handler(AgentPid::pid()) -> {ok, pid()}.
get_handler(AgentPid) ->
	case catch gproc:lookup_pid({n, g, {?MODULE,AgentPid}}) of
		Handler when is_pid(Handler) ->
			lager:debug("Found handler for agent ~p: ~p", [AgentPid, Handler]),
			{ok, Handler};
		_ ->
			lager:debug("Starting new handler for agent ~p", [AgentPid]),
			?MODULE:start_link(AgentPid)
	end.

-spec join_call(pid(), string(), string(), string(), atom()) -> ok | {error, binary(), binary()}.
join_call(Pid, Type, Agent, ChanId, ChanPid) ->
	gen_server:call(Pid, {join_call, Agent, ChanId, ChanPid, Type}).

%% gen_server callbacks

init([AgentPid]) ->
	Agent = agent:dump_state(AgentPid),
	Login = Agent#agent.login,
	Connection = Agent#agent.connection,
	gproc:reg({n, g, {?MODULE, AgentPid}}),
	{ok, #state{agent_pid=AgentPid, login=Login, connection=Connection}}.

handle_call({join_call, CallAgent, ChanId, ChanPid, Type}, _From, State) ->
	Agent = agent:dump_state(State#state.agent_pid),
	lager:debug("Trying to join the call on channel ~p for agent ~p", [ChanId, CallAgent]),
	Result = do([error_m ||
		% CallAgent <- get_call_agent(Call),
		GenMedia <- get_media(ChanPid),
		MonitorLeg <- get_monitor_leg(GenMedia),
		CallRec <- get_call_rec(GenMedia),
		check_released(Agent),
		CallerIdArg = CallRec#call.callerid,
		CustomDialStringArgs = case CallerIdArg of
			undefined ->
				?DEFAULT_DIALSTRING_ARGS;
			{CallerId, CallerNum} ->
				["origination_caller_id_name='" ++ CallerId ++ "'", "origination_caller_id_number='" ++ CallerNum ++ "'"]
		end,
		DialString <- get_fs_dialstring(Agent, CustomDialStringArgs),
		lager:debug("Monitoring call ~p of agent ~p", [MonitorLeg, CallAgent]),
		initiate_monitor_call(Type, State#state.monitored_call, MonitorLeg, CallAgent, DialString)
	]),
	Reply = case Result of
		ok ->
			{ok, initiated};
		{error, Reason} ->
			{error, Reason}
	end,
	{reply, Reply, State#state{channel_id = ChanId}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({sup_monitor_result, {Type, FSNode, CallId, Agent}, {Status, Reply}}, State) ->
	ChanId = State#state.channel_id,
	{CallIdKey, ResultEvent} = case Type of
		monitor ->
			{monitor_callid, ouc_call_monitor_result};
		barge ->
			{barge_callid, ouc_call_barge_result}
	end,
	ReplyList = string:tokens(Reply, " \n"),
	{Result, State2} = case {Status, ReplyList} of
		{ok, ["+OK"|Rest]} ->
			OldCall = State#state.monitored_call,
			MonitorCallId = case {OldCall =/= undefined, Rest} of
				{true, _} ->
					EndEvent = case OldCall#monitored_call.type of
						monitor -> ouc_call_monitor_ended;
						barge -> ouc_call_barge_ended
					end,
					State#state.connection ! {EndEvent, {l2b(OldCall#monitored_call.agent), l2b(OldCall#monitored_call.channelid), [{CallIdKey, l2b(OldCall#monitored_call.callid)}]}},
					OldCall#monitored_call.monitor_callid;
				{_, [Uuid]} ->
					Uuid
			end,
			freeswitch_media_manager:notify(MonitorCallId, self()),
			Data = [{CallIdKey, l2b(MonitorCallId)}],
			MonitoredCall = #monitored_call{monitor_callid = MonitorCallId, callid = CallId, channelid = ChanId, agent = Agent, type = Type, node = FSNode},
			{{success, Data}, State#state{monitored_call = MonitoredCall}};
		{error, ["-ERR", "UNALLOCATED_NUMBER"]} ->
			{{error, [{agent, l2b(Agent)}, {code, <<"4001">>}, {message, <<"Ring init on endpoint failed">>}]}, State};
		{error, ["-ERR", "CALL_REJECTED"]} ->
			{{error, [{agent, l2b(Agent)}, {code, <<"4006">>}, {message, <<"Call was rejected by the supervisor">>}]}, State};
		_Els ->
			{{error, [{agent, l2b(Agent)}, {code, <<"4007">>}, {message, l2b(Reply)}]}, State}
	end,
	State#state.connection ! {ResultEvent, {l2b(Agent), l2b(ChanId), Result}},
	{noreply, State2};
handle_info(channel_destroy, State) ->
	MonitoredCall = State#state.monitored_call,
	{CallIdKey, EndEvent} = case MonitoredCall#monitored_call.type of
		monitor ->
			{monitor_callid, ouc_call_monitor_ended};
		_ ->
			{barge_callid, ouc_call_barge_ended}
	end,
	Data = [{CallIdKey, l2b(MonitoredCall#monitored_call.callid)}],
	State#state.connection ! {EndEvent, {l2b(MonitoredCall#monitored_call.agent), l2b(MonitoredCall#monitored_call.channelid), Data}},
	{noreply, State#state{monitored_call = undefined}};
handle_info(_Info, State) ->
	lager:debug("Got unhandled info ~p", [_Info]),
	{noreply, State}.
terminate(_Reason, State) ->
	MonitoredCall = State#state.monitored_call,
	case MonitoredCall of
		undefined ->
			ok;
		_ ->
			freeswitch:bgapi(MonitoredCall#monitored_call.node, uuid_kill, MonitoredCall#monitored_call.monitor_callid)
	end,
	ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% internal functions

% get_call_agent(CallId) ->
% 	Agents = qlc:e(qlc:q([Login || {_, _, #cpx_agent_channel_prop{login=Login, callid=PropCallId}} <-
% 		gproc:table({g, p}), PropCallId =:= CallId])),
% 	case Agents of
% 		[Agent|_] ->
% 			{ok, Agent};
% 		_ ->
% 			{error, call_not_found}
% 	end.

get_media(ChanPid) ->
	case agent_channel:get_media(ChanPid) of
		{ok, undefined} ->
			{error, call_not_found};
		{ok, GenMedia} ->
			{ok, GenMedia};
		_ -> {error, call_not_found}
	end.

get_monitor_leg(GenMedia) ->
	case gen_media:get_monitor_leg(GenMedia) of
		{ok, undefined} ->
			{error, call_not_found};
		{ok, Call} ->
			{ok, Call};
		_ -> {error, call_not_found}
	end.

get_call_rec(GenMedia) ->
  case gen_media:get_call(GenMedia) of
    undefined ->
      {error, call_not_found};
    CallRec ->
      {ok, CallRec}
  end.

check_released(Agent) ->
	case Agent#agent.release_data of
		undefined ->
			{error, invalid_agent_state};
		_ ->
			ok
	end.

get_fs_dialstring(Agent, DialStringArgs) ->
	case agent:get_endpoint(freeswitch_media, Agent) of
		{ok, {_, {freeswitch_ring, EndpointData}}} ->
			FSNode = proplists:get_value(freeswitch_node, EndpointData),
			Destination = proplists:get_value(destination, EndpointData),
			DialstringFmt = proplists:get_value(dialstring, EndpointData),
			Dialstring = freeswitch_media_manager:do_dial_string(DialstringFmt, Destination, DialStringArgs),
			{ok, {FSNode, Dialstring}};
		_ ->
			{error, no_endpoint}
	end.

initiate_monitor_call(Type, undefined, CallId, Agent, {FSNode, Dialstring}) ->
	FSApp = case Type of
		monitor -> eavesdrop;
		barge -> three_way
	end,

	lager:debug("Calling originate with args ~p", [Dialstring ++ " &" ++ atom_to_list(FSApp) ++ "(" ++ CallId ++ ");"]),
	case freeswitch:bgapi(FSNode, originate, Dialstring ++ " &" ++ atom_to_list(FSApp) ++ "(" ++ CallId ++ ");", monitor_callback(Type, FSNode, CallId, Agent)) of
		ok ->
			ok;
		{ok, _} ->
			ok;
		_ ->
			{error, unable_to_initiate}
	end;
initiate_monitor_call(Type, MonitoredCall, CallId, Agent, {FSNode, _Dialstring}) ->
	FSApp = case Type of
		monitor -> eavesdrop;
		barge -> three_way
	end,
	lager:debug("Calling uuid_transfer with args ~p",[lists:flatten(MonitoredCall#monitored_call.monitor_callid ++ " '" ++ atom_to_list(FSApp) ++ ":" ++ CallId ++ "' inline")]),
	case freeswitch:bgapi(FSNode, uuid_transfer, lists:flatten(MonitoredCall#monitored_call.monitor_callid ++ " '" ++ atom_to_list(FSApp) ++ ":" ++ CallId ++ "' inline"), monitor_callback(Type, FSNode, CallId, Agent)) of
		ok ->
			ok;
		{ok, _} ->
			ok;
		_ ->
			{error, unable_to_initiate}
	end.

monitor_callback(App, FSNode, CallId, Agent) ->
	Self = self(),
	fun(Status, Reply) ->
		Self ! {sup_monitor_result, {App, FSNode, CallId, Agent}, {Status, Reply}}
	end.

-ifdef(TEST).

handle_test_() ->
	State = #state{},
	{setup, fun() ->
		meck:new(agent),
		meck:new(freeswitch),
		meck:expect(agent, dump_state, 1, #agent{login="agent"}),
		application:start(gproc)
	end, fun(_) ->
		meck:unload()
	end, [{"call not found", fun() ->
		?assertEqual({reply, {error, call_not_found}, State}, handle_call({join_call, "uuid", monitor}, from, State))
	end}, {"supervisor not released", fun() ->
		spawn_link(
			fun() ->
				gproc:add_global_property(cpx_agent_channel, #cpx_agent_channel_prop{login="agent", callid="uuid"}),
				receive kill -> ok end
			end
		),
		timer:sleep(100),
		?assertEqual({reply, {error, invalid_agent_state}, State}, handle_call({join_call, "uuid", monitor}, from, State))
	end}, {"no endpoint", fun() ->
		meck:expect(agent, dump_state, 1, #agent{login="agent", release_data = ?DEFAULT_RELEASE}),
		meck:expect(agent, get_endpoint, 2, {error, none}),
		?assertEqual({reply, {error, no_endpoint}, State}, handle_call({join_call, "uuid", monitor}, from, State))
	end}, {"monitor success", fun() ->
		meck:expect(agent, get_endpoint, 2,
			{ok, {[], {freeswitch_ring, [{freeswitch_node,'freeswitch@127.0.0.1'},
				{dialstring,"{ignore_early_media=true}sofia/oacddev.ezuce.com/$1;sipx-noroute=VoiceMail;sipx-userforward=false"},
				{destination,"agent@oacddev.ezuce.com"}]}}}),
		meck:expect(freeswitch, bgapi, 4, ok),
		?assertEqual({reply, {ok, initiated}, State}, handle_call({join_call, "uuid", monitor}, from, State)),
		FSArgs = ['freeswitch@127.0.0.1', originate,
			"{origination_caller_id_name='Reach',origination_caller_id_number='Reach',ignore_early_media=true}sofia/oacddev.ezuce.com/agent@oacddev.ezuce.com;sipx-noroute=VoiceMail;sipx-userforward=false &eavesdrop(uuid);", '_'],
		?assert(meck:called(freeswitch, bgapi, FSArgs))
	end}]}.

-endif.
