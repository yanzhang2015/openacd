-module(ouc_calls).

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("oacd_ouc.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("reach_core/include/call.hrl").
-include_lib("reach_core/include/queue.hrl").
-include_lib("reach_core/include/gen_media.hrl").
-include_lib("reach_core/include/agent.hrl").

-ifndef(TEST).
-define(CHECK_INTERVAL, 1000).
-else.
-define(CHECK_INTERVAL, 100).
-endif.

-define(SERVER, {local, ?MODULE}).
-define(SUBSCRIBE_KEY, {p, g, ouc_queued_calls_update}).

-import(cpx_json_util, [b2l/1]).

%% API
-export([start/0, start_link/0, stop/0,
	get_queued_calls/0, get_queued_calls/1, subscribe/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {queued_calls, check_flag, check_timer}).

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

-spec get_queued_calls() -> [#qcall{}].
get_queued_calls() ->
	get_queued_calls([]).

-spec get_queued_calls(list()) -> [#qcall{}].
get_queued_calls(Opts) ->
	gen_server:call(?MODULE, {get_queued_calls, Opts}).

-spec subscribe() -> ok.
subscribe() ->
	catch gproc:reg(?SUBSCRIBE_KEY),
	ok.

%% gen_server callbacks
init([]) ->
	gproc:add_global_property(cpx_media_change, subscribe),
	gproc:add_global_property(cpx_agent_change, subscribe),
	QueuedCalls = lookup_queued_calls(),
	TimerRef = start_check_timer(),
	{ok, #state{queued_calls=QueuedCalls, check_flag=false, check_timer=TimerRef}}.

handle_call({get_queued_calls, Opts}, _From, State) ->
	QueuedCalls = case proplists:get_value(force, Opts) of
		true ->
			lookup_queued_calls();
		_ ->
			State#state.queued_calls
	end,
	{reply, QueuedCalls, State#state{queued_calls = QueuedCalls}};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
	{reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(#cpx_gen_media_init{}, State) ->
	lager:debug("Received gen_media init notification, setting check_flag to true"),
	{noreply, State#state{check_flag=true}};
handle_info(#cpx_gen_media_update{}, State) ->
	lager:debug("Received gen_media update notification, setting check_flag to true"),
	{noreply, State#state{check_flag=true}};
handle_info(#cpx_agent_login{}, State) ->
	lager:debug("Received agent login notification, setting check_flag to true"),
	{noreply, State#state{check_flag=true}};
handle_info(#cpx_agent_state_update{}, State) ->
	lager:debug("Received agent state update notification, setting check_flag to true"),
	{noreply, State#state{check_flag=true}};
handle_info(#cpx_agent_logout{}, State) ->
	lager:debug("Received agent logout notification, setting check_flag to true"),
	{noreply, State#state{check_flag=true}};
handle_info({timeout, Timer, check}, State = #state{check_timer=Timer}) ->
	Timer2 = start_check_timer(),
	Calls = case State#state.check_flag of
		true ->
			OCalls = State#state.queued_calls,
			NCalls = lookup_queued_calls(),

			Diffs = (catch get_list_diffs(OCalls, NCalls, [])),
			send_list_diffs(Diffs),

			NCalls;
		_ ->
			State#state.queued_calls
	end,
	State1 = State#state{queued_calls=Calls, check_timer=Timer2, check_flag=false},
	{noreply, State1};
handle_info(_Info, State) ->
	% lager:debug("Received unhandled info ~p", [_Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

%% Internal
start_check_timer() ->
	erlang:start_timer(?CHECK_INTERVAL, self(), check).

-type list_diff() :: #qcall{} | {call_delete, Id::string()}.
-spec get_list_diffs(Old::[#qcall{}], New::[#qcall{}], [list_diff()]) -> [list_diff()].
get_list_diffs([], [], Acc) ->
	lists:reverse(Acc);
get_list_diffs([H|T1], [H|T2], Acc) ->
	get_list_diffs(T1, T2, Acc);
get_list_diffs([#qcall{id=Id}|T1], [#qcall{id=Id}=H2|T2], Acc) ->
	get_list_diffs(T1, T2, [H2|Acc]);

% %% Deleted old queue
get_list_diffs([#qcall{id=Id1}|T1], [#qcall{id=Id2}|_]=L2, Acc) when Id1 < Id2 ->
	get_list_diffs(T1, L2, [{call_delete, Id1}|Acc]);
get_list_diffs([#qcall{id=Id1}|T1], [], Acc) ->
	get_list_diffs(T1, [], [{call_delete, Id1}|Acc]);

%% New queue
get_list_diffs([#qcall{id=Id1}|_]=L1, [#qcall{id=Id2}=H2|T2], Acc) when Id1 > Id2 ->
	get_list_diffs(L1, T2, [H2|Acc]);
get_list_diffs([], [H2|T2], Acc) ->
	get_list_diffs([], T2, [H2|Acc]).

send_list_diffs([]) ->
	ok;
send_list_diffs(Diffs) ->
    % lager:debug("sending Diffs: ~p",[Diffs]),
	gproc:send({p, g, ouc_queued_calls_update},
		{ouc_queued_calls, Diffs}).

%% ---------------------------------------------------------------------------
-spec lookup_queued_calls() -> [#qcall{}].
%% @private
%% @doc Lookups a list of queued calls stored in gproc 
%% @end
%% ---------------------------------------------------------------------------
lookup_queued_calls() ->
	CallRecs = qlc:e(qlc:q([{CallPid, Prop} ||
		{_, CallPid, #cpx_gen_media_prop{state=State} = Prop} <- gproc:table({g, p}), lists:member(State, [inqueue, inqueue_ringing])])),
	%% lager:debug("Got queued calls ~p", [CallRecs]),
	Channels = get_agent_channels(),
	lists:sort([get_queued_call(CallRec, CallPid, Channels) || {CallPid, CallRec} <- CallRecs]).

%% ----------------------------------------------------------------------------
-spec get_queued_call(MediaProp::term(), CallPid::pid(), Channels::term()) -> #qcall{}.
%% @private
%% @doc Returns structured information about a queued call
%% @end
%% ----------------------------------------------------------------------------

get_queued_call(MediaProp, CallPid, Channels) ->
	Rec = MediaProp#cpx_gen_media_prop.call,
	Id = Rec#call.id,
	Type = Rec#call.type,
	Dnis = Rec#call.dnis,
	Queue = Rec#call.queue,
	SourceModule = Rec#call.source_module,
	Line = get_line_info(Dnis),
	% Skills = case Rec#call.skills of
	% 	[] ->
	% 		get_queue_skills(Queue);
	% 	Ss ->
	% 		Ss
	% end,
	Skills = Rec#call.skills,
	{TotalAgMatch, AvAgMatch, IdleAgMatch} = count_agent_matches(Skills, Channels),
	CallerId = Rec#call.callerid,
	Client = Rec#call.client#client.label,
	StateChanges = MediaProp#cpx_gen_media_prop.state_changes,
	Timestamp = case proplists:get_value(inqueue, StateChanges) of
		undefined ->
			{_, T} = lists:nth(1, StateChanges),
			T;
		Other ->
			Other
	end,
	TimeQueued = util:now_ms(Timestamp),
    
    GenMediaNode = ouc_utils:get_node(CallPid),
	#qcall{id=Id,
           time_queued=TimeQueued,
           type=Type,
           reach_node=GenMediaNode,
		   total_agent_count=TotalAgMatch,
           available_agent_count = AvAgMatch,
		   idle_agent_count=IdleAgMatch,
           dnis=Dnis,
           line=Line, queue=Queue,
		   skills=Skills,
           callerid=CallerId,
           client=Client,
		   source_module=SourceModule}.

count_agent_matches(Skills, Channels) ->
	Agents = qlc:e(qlc:q([{Login, AgState} || {_, _, #cpx_agent_prop{login=Login, skills=AgSkills, state=AgState}} <- gproc:table({g, p}),
		lists:member('_all', AgSkills) orelse
		lists:member('_all', Skills) orelse
		util:list_contains_all(AgSkills, Skills)])),
	AvailableAgents = [Login || {Login, AgState} <- Agents, AgState =:= available],
	IdleAgents = [Login || Login <- AvailableAgents, not proplists:is_defined(Login, Channels)],
	lager:debug("Call matched agents ~p", [proplists:get_keys(Agents)]),
	lager:debug("Call matched available agents ~p", [IdleAgents]),
	{length(Agents), length(AvailableAgents), length(IdleAgents)}.

get_agent_channels() ->
	qlc:e(qlc:q([{Login, State} ||
		{_, _, #cpx_agent_channel_prop{login=Login, state=State}} <- gproc:table({g, p}), State =/= stop])).

get_line_info(Line) ->
	Db = ouc_db:get_db(imdb),
	{ok, Props} = Db:findOne(<<"entity">>, [{<<"ent">>, <<"openacdline">>}, {<<"ident">>, {regex, "^" ++ Line ++ "@.*", "i"}}]),
	case ej:get({"linm"}, Props, null) of
		null ->
			undefined;
		N ->
			b2l(N)
	end.

% get_queue_skills(Queue) ->
% 	QueueSkills = qlc:e(qlc:q([Skills || {_, _, #cpx_call_queue_prop{name = PropQueue, skills = Skills}} <- gproc:table({g, p}), Queue =:= PropQueue])),
% 	case QueueSkills of
% 		[Ss|_] ->
% 			Ss;
% 		_ ->
% 			[]
% 	end.

-ifdef(TEST).

t_media({Caller, Queue, Line, Client, InqueueTs, Skills}, Notify) ->
	CallRec = #call{id="id"++Caller, source=Caller, type=voice, callerid={Caller, Caller}, queue=Queue, dnis=Line, skills=Skills, client=#client{label=Client}},
	MediaPid = spawn_link(fun() ->
		MediaProp = #cpx_gen_media_prop{call=CallRec, state=inqueue, state_changes=[{inqueue, InqueueTs}, {inivr, {13,12,8}}, {init,{13,12,7}}]},
		gproc:add_global_property(cpx_media, MediaProp),
		case Notify of
			true ->
				gproc:send({p, g, cpx_media_change}, #cpx_gen_media_update{});
			_ ->
				ok
		end,
		receive kill -> ok end
	end),
	MediaPid.

t_agent(AgState, Skills, Notify) ->
	spawn_link(fun() ->
		gproc:reg({p, g, cpx_agent}, #cpx_agent_prop{state=AgState, skills=[random | Skills]}),
		case Notify of
			true ->
				gproc:send({p, g, cpx_agent_change}, #cpx_agent_login{});
			_ ->
				ok
		end,
		receive kill -> ok end
	end).

t_samp_line() ->
	[{<<"_id">>,<<"OpenAcdLine6">>},
		{<<"ident">>,<<"6@oacddev.ezuce.com">>},
		{<<"ent">>,<<"openacdline">>},
		{<<"linm">>,<<"Line6">>},
		{<<"qnm">>, <<"sales">>}].

get_queued_calls_test_() ->
	{spawn, {setup, fun() ->
		application:start(gproc),
		call_queue_config:start(),
		call_queue_config_ets:load_queues(["sales"]),

		meck:new(mongoapi),
		meck:expect(mongoapi, new, fun(N, Db) -> {mongoapi, N, Db} end),
		meck:expect(mongoapi, findOne, 3, {ok, t_samp_line()}),

		start()
	end, fun(_) ->
		stop(),
		meck:unload()
	end, [{"get queue calls", fun() ->
		A1 = t_agent(available, ['english', {'_brand', "Client 1"}], false),
		A2 = t_agent(released, ['english', {'_brand', "Client 1"}], false),

		M1 = t_media({"6001", "sales", "6", "client1", {13,10,10}, ['english', {'_brand', "Client 1"}]}, false),
		M2 = t_media({"6002", "sales", "6", "client2", {13,11,11}, ['english', {'_brand', "Client 2"}]}, false),
		timer:sleep(110),

		?assertEqual(lists:sort([#qcall{id = "id6001", time_queued = 13000010000, dnis="6", line = "Line6", queue="sales", skills=['english', {'_brand', "Client 1"}], callerid = {"6001", "6001"}, client = "client1", type = voice, total_agent_count = 2, available_agent_count = 1, idle_agent_count = 1},
			#qcall{id = "id6002", time_queued = 13000011000, dnis="6", line = "Line6", queue="sales", skills=['english', {'_brand', "Client 2"}], callerid = {"6002", "6002"}, client = "client2", type = voice, total_agent_count = 0, available_agent_count = 0, idle_agent_count = 0}]), get_queued_calls([{force, true}])),

		M1 ! kill,
		M2 ! kill,
		A1 ! kill,
		A2 ! kill
	end}, {"get queue call diffs", fun() ->
		get_queued_calls([{force, true}]),
		gproc:reg({p, g, ouc_queued_calls_update}),

		M3 = t_media({"6003", "sales", "6", "client3", {13,12,12}, ['english', {'_brand', "Client 3"}]}, true),
		?assertEqual([#qcall{id = "id6003", time_queued = 13000012000, dnis="6", line = "Line6", queue="sales",
			skills=['english', {'_brand', "Client 3"}], callerid = {"6003", "6003"}, client = "client3", type = voice, total_agent_count = 0, available_agent_count = 0, idle_agent_count = 0}],
			receive {ouc_queued_calls, Diff} -> Diff after 500 -> diff_not_received end),

		A1 = t_agent(available, ['english', {'_brand', "Client 3"}], true),
		?assertEqual([#qcall{id = "id6003", time_queued = 13000012000, dnis="6", line = "Line6", queue="sales",
			skills=['english', {'_brand', "Client 3"}], callerid = {"6003", "6003"}, client = "client3", type = voice, total_agent_count = 1, available_agent_count = 1, idle_agent_count = 1}],
			receive {ouc_queued_calls, Diff} -> Diff after 500 -> diff_not_received end),
		M3 ! kill,
		A1 ! kill
	end}]}}.

-endif.
