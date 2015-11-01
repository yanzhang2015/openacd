% -module(freeswitch_outbound).

% %% API
% -export([start/3]).

% start(FSNode, Agent, Dest) ->
% 	CallerId = "OpenACD",
% 		freeswitch:api(FSNode, expand,
% 		" originate {origination_caller_id_number=" ++ CallerId ++
% 		"}sofia/${domain}/" ++ Agent ++
% 		"@${domain} &bridge({origination_caller_id_number=" ++ Agent ++
% 		"}sofia/${domain}/" ++
% 		Dest ++ "@${domain})").

-module(freeswitch_outbound).

-behaviour(gen_fsm).

-include_lib("reach_core/include/agent.hrl").
-include_lib("reach_core/include/call.hrl").
-include_lib("reach_core/include/queue.hrl").

%% API
-export([start_link/2,
         agent_pickup/1,
         call_destination/2,
         outbound_pickup/1,
         hold/1,
         unhold/1,
         hold_update/2,
         stop/1,
         handle_queue_transfer/5,
         handle_conference_to_queue/6,
         get_bleg/1,
         transfer/2,
         conference/2]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3,
         agent_ringing/2, awaiting_destination/2,
         outbound_ringing/2, oncall/2,
         hold/2,
         awaiting_destination/3, oncall/3,
         handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-define(EVENT_KEY, outbound_call).
-define(ERR_NO_RESPONSE, "NO_USER_RESPONSE").
-define(ERR_NO_PICKUP, "UNALLOCATED_NUMBER").

-import(cpx_json_util, [l2b/1, b2l/1, nob/1]).

-record(state, {
        uuid,
        bleg,
        fnode,
        conn,
        agent,
        agent_pid,
        destination,
        client,
        client_opts,
        rec_path,
        type,
        state_changes,
        parent,
        parentmon,
        conf_id,
        orig_call,
        caller_id,
        moh = "moh" :: string() | 'none',
        transferred = false :: boolean(),
        is_conference = false :: boolean()
  }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%%--------------------------------------------------------------------
start_link(Cnode, Props) ->
  gen_fsm:start_link(?MODULE, [Cnode, Props], []).

agent_pickup(Pid) ->
    lager:debug("In agent_pickup API", []),
    gen_fsm:send_event(Pid, agent_pickup).

call_destination(Pid, Client) ->
    lager:debug("In call_destination API with values ~p ~p", [Pid, Client]),
    gen_fsm:sync_send_event(Pid, {call_destination, Client}, infinity).

outbound_pickup(Pid) ->
    lager:debug("In outbound_pickup API" , []),
    gen_fsm:send_event(Pid, outbound_pickup).

hold(Pid) ->
    gen_fsm:send_event(Pid, hold).

unhold(Pid) ->
    gen_fsm:send_event(Pid, unhold).

hold_update(Pid, HoldStatus) ->
    gen_fsm:send_event(Pid, {hold_update, HoldStatus}).

transfer(Pid, Dest) ->
    gen_fsm:send_event(Pid, {transfer, Dest}).

conference(Pid, Dest) ->
    gen_fsm:send_event(Pid, {conference, Dest}).

stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

get_bleg(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, get_bleg).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to
%% initialize.
%%--------------------------------------------------------------------
init([Fnode, Props]) ->
    Type = proplists:get_value(type, Props),
    case Type of
        voicemail ->
            process_voicemail(Fnode, Props);
        conference ->
            process_conference(Fnode, Props);
        _ ->
            process_outbound(Fnode, Props)
    end.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName,
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also
%% called if a timeout occurs.
%%--------------------------------------------------------------------
agent_ringing(agent_pickup, #state{
        fnode=Fnode, uuid=UUID, conn=Conn} = State) ->
    send_update(Conn, UUID, awaiting_destination),
    freeswitch:bgapi(Fnode, uuid_broadcast, UUID ++ " local_stream://moh"),
    {next_state, awaiting_destination, State};

agent_ringing(Event, State) ->
    lager:info("unhandled event ~p while in state ~p", [Event, agent_ringing]),
    {next_state, agent_ringing, State}.

awaiting_destination(Event, State) ->
    lager:info("unhandled event ~p while in state ~p", [Event, outbound_ringing]),
    {next_state, awaiting_destination, State}.

outbound_ringing(outbound_pickup, #state{
        fnode=Fnode, uuid=UUID, bleg=BLeg,
        type=conference, conf_id=ConfId, orig_call=Call} = State) ->
    lager:debug("In outbound_pickup state", []),
    BridgeOutcome = freeswitch:api(Fnode, uuid_transfer,
    " " ++ BLeg ++
    " conference:" ++ UUID ++ "@reach inline"),
    lager:debug("Bridge result : ~p", [BridgeOutcome]),
    send_conf_update(Call, ConfId, accepted),
    {next_state, oncall, State};

outbound_ringing(outbound_pickup, #state{
        fnode=Fnode, uuid=UUID, bleg=BLeg, state_changes=StateChanges,
        conn=Conn, type = Type, client=Client, orig_call=OrigCall} = State) ->
    lager:debug("In outbound_pickup state ~p", [State]),
    BridgeOutcome = freeswitch:api(Fnode, uuid_bridge,
    " " ++ BLeg ++
    " " ++ UUID),
    lager:debug("Bridge result : ~p", [BridgeOutcome]),
    InitTime = proplists:get_value(init, StateChanges, os:timestamp()),
    case BridgeOutcome of
        {ok, _} ->
            CallId = case Type of
              voicemail -> OrigCall#call.id;
              conference -> OrigCall#call.id;
              _ -> BLeg
            end,
            ClientId = case Client of
                         C when is_record(Client, client) -> C#client.id;
                         _ -> undefined
                       end,
            RecPath = case cpx_recordings:get_path(CallId, InitTime, ClientId, true) of
                undefined ->
                    lager:info("archiving is not configured for ~p, client ~p", [CallId, Client]),
                    undefined;
                Path ->
                    lager:info("archiving ~p to ~p", [CallId, Path]),
                    freeswitch:api(Fnode, uuid_setvar_multi, CallId ++
                        " RECORD_APPEND=true;recording_follow_transfer=true"),
                    freeswitch:api(Fnode, uuid_record, CallId ++
                        " start " ++ Path),
                    Path
            end,
            StateChanges1 = [{oncall, os:timestamp()} | StateChanges],
            send_update(Conn, UUID, oncall),
            {next_state, oncall, State#state{rec_path=RecPath, state_changes=StateChanges1}};
        {error, Error} ->
            lager:info("Bridge error: ~p", [Error]),
            {stop, normal, ended}
    end;

outbound_ringing(Event, State) ->
    lager:info("unhandled event ~p while in state ~p", [Event, outbound_ringing]),
    {next_state, outbound_ringing, State}.

oncall(hold, #state{uuid = Uuid, fnode = Node} = State) ->
    freeswitch:api(Node, uuid_phone_event, Uuid ++ " hold"),
    {next_state, hold, State};

oncall({hold_update, hold}, State) ->
    {next_state, hold, State};

oncall({transfer, Destination0}, State) ->
    Destination = freeswitch_media_manager:fix_sip_addr(Destination0),
    #state{fnode = Fnode, bleg = UUID} = State,
    BaseDS = freeswitch_media_manager:get_default_dial_string(),
    RingOpts =  ["hangup_after_bridge=true"],

    lager:debug("Transfering outbound call to ~p, state ~p", [Destination0, State]),
    Dialstring = freeswitch_media_manager:do_dial_string_without_sip_opts(BaseDS, Destination, RingOpts),
    freeswitch:bgapi(Fnode, uuid_setvar, UUID ++ " transfer_ringback local_stream://" ++ State#state.moh),
    freeswitch:bgapi(Fnode, uuid_transfer, UUID ++ " 'm:^:bridge:" ++ Dialstring ++ "' inline"),

    {next_state, oncall, State#state{transferred = true}};

oncall({conference, Destination0}, State) ->
    #state{fnode = Fnode, uuid = AgentLeg, bleg = UUID} = State,
    case State#state.is_conference of
        false ->
            freeswitch:bgapi(Fnode, uuid_setvar, UUID ++ " hangup_after_bridge false"),
            freeswitch:bgapi(Fnode, uuid_setvar, AgentLeg ++ " hangup_after_bridge false"),
            freeswitch:bgapi(Fnode, uuid_transfer, UUID ++ " -both conference:" ++
                UUID ++ "@reach++flags{endconf} inline");
        _ ->
            ok
    end,
    originate_conference(Fnode, Destination0, UUID ++ "@reach"),
    {next_state, oncall, State#state{is_conference = true}};

oncall(Event, State) ->
    lager:info("unhandled event ~p while in state ~p", [Event, oncall]),
    {next_state, oncall, State}.

hold(unhold, #state{uuid = Uuid, fnode = Node} = State) ->
    freeswitch:api(Node, uuid_phone_event, Uuid ++ " talk"),
    {next_state, oncall, State};

hold({hold_update, undefined}, State) ->
    {next_state, oncall, State};

hold(Event, State) ->
    lager:info("unhandled event ~p while in state ~p", [Event, hold]),
    {next_state, hold, State}.



state_name(_Event, State) ->
  {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName,
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName,
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------

awaiting_destination({call_destination, Dest}, _From,
    #state{fnode=Fnode, uuid=UUID, conn=Conn,
           agent=Agent, client_opts = ClientOpts} = State) ->

    lager:notice("Client options in awaiting destination ~p", [ClientOpts]),
    {CallerId, CallerNum} = case ClientOpts of
                 undefined ->
                   {Agent, Agent};
                 _ ->
                   case proplists:get_bool(use_agent_callerid, ClientOpts) of
                     true -> {Agent, Agent};
                     false ->
                        BinOutCallerId = proplists:get_value(outbound_caller_id, ClientOpts),
                        OutCallerId = lists:flatten(binary_to_list(BinOutCallerId)),
                        EscapedOutCallerId = freeswitch_util:escape_string(OutCallerId),
                        {EscapedOutCallerId, Agent}
                   end
               end,

    lager:notice("CallerId  ~p", [CallerId]),
    case freeswitch:api(Fnode, create_uuid) of
        {ok, BLeg} ->
            originate(Fnode, BLeg, Dest, CallerId, CallerNum),
            send_update(Conn, UUID, outgoing_ringing),
            lager:debug("In call_destination state with bleg ~p", [BLeg]),
            {reply, ok, awaiting_destination,
                State#state{destination = Dest, bleg = BLeg, caller_id = {CallerId, CallerNum}}};
        _ ->
            {stop, uuid_not_created}
    end;

awaiting_destination(Event, _From, State) ->
    lager:info("unhandled event ~p while in state ~p", [Event, awaiting_destination]),
    {reply, ok, awaiting_destination, State}.

oncall(Event, _From, State) ->
    lager:info("unhandled event ~p while in state ~p", [Event, oncall]),
    {reply, {error, existing_call}, oncall, State}.

state_name(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_event(Event, StateName, State) -> {next_state, NextStateName,
%%                                                NextState} |
%%                                          {next_state, NextStateName,
%%                                                NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(stop, _StateName, #state{type = conference, conf_id=ConfId,
    state_changes=StateChanges, orig_call=Call} = State) ->
    StateChanges1 = [{wrapup, os:timestamp()} | StateChanges],
    store_recording_info(State#state{state_changes=StateChanges1}),
    send_conf_update(Call, ConfId, ended),
    drop_bleg(State),
    lager:debug("Received call_hangup", []),
    {stop, normal, State#state{state_changes=StateChanges1}};

handle_event(stop, _StateName, #state{type = voicemail,
    state_changes=StateChanges} = State) ->
    StateChanges1 = [{wrapup, os:timestamp()} | StateChanges],
    store_recording_info(State#state{state_changes=StateChanges1}),
    end_call(State),
    lager:debug("Received call_hangup", []),
    {stop, normal, State#state{state_changes=StateChanges1}};

handle_event(stop, _StateName, #state{state_changes=StateChanges} = State) ->
    StateChanges1 = [{wrapup, os:timestamp()} | StateChanges],
    store_recording_info(State#state{state_changes=StateChanges1}),
    end_call(State),
    lager:debug("Received call_hangup", []),
    {stop, normal, State#state{state_changes=StateChanges1}};

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_sync_event(Event, From, StateName,
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState,
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState,
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(get_bleg, _From, StateName, State) ->
  {reply, State#state.bleg, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState,
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
% handle_info({call, {event, [UUID | Rest]}}, _StateName, Call, _Internal, State) when is_list(UUID) ->
%   SetSess = freeswitch:session_setevent(State#state.cnode, [
%     'CHANNEL_BRIDGE', 'CHANNEL_PARK', 'CHANNEL_HANGUP',
%     'CHANNEL_HANGUP_COMPLETE', 'CHANNEL_DESTROY', 'DTMF',
%     'CHANNEL_ANSWER', 'CUSTOM', 'conference::maintenance']),
%   lager:debug("reporting new call ~p (eventage:  ~p).", [UUID, SetSess]),
%   case State#state.uuid of
%     UUID -> freeswitch_media_manager:notify(UUID, self());
%     _ -> ok
%   end,
%   case_event_name([UUID | Rest], Call, State#state{in_control = true});

handle_info({call_event, {event, [UUID | EventPropList]}}, StateName, State) ->
    case_event_name([UUID|EventPropList], StateName, State);

handle_info({call, {event, [UUID | _EventPropList]}}, agent_ringing,
        #state{uuid=UUID} = State) ->
    agent_pickup(self()),
    lager:debug("Call established in ~p", [self()]),
    {next_state, agent_ringing, State};

handle_info({call, {event, [BLeg | _EventPropList]}}, outbound_ringing,
        #state{bleg=BLeg} = State) ->
    lager:debug("BLeg established", []),
    outbound_pickup(self()),
    {next_state, outbound_ringing, State};

handle_info({error, Error}, _StateName, State) ->
    lager:debug("Error received: ~p", [Error]),
    {stop, Error, State};

handle_info({bgerror, _MsgID, Error}, StateName, State) ->
    UUID = State#state.uuid,
    Conn = State#state.conn,
    Call = State#state.orig_call,
    ConfId = State#state.conf_id,
    Type = State#state.type,
    Parent = State#state.parent,
    lager:debug("Error received: ~p", [Error]),
    ErrorTok = string:tokens(Error, " \n"),
    {Action, Event} = case ErrorTok of
        ["-ERR"|[ErrorList]] when is_list(ErrorList) ->
            {Reply, NextState, _Msg} =
                case StateName of
                    precall ->
                        send_update(Conn, UUID, ended, cpx_json_util:l2b(ErrorList)),
                        {stop, normal, ended};
                    _ ->
                        case Type of
                            voicemail ->
                                send_update(Conn, UUID, ended, cpx_json_util:l2b(ErrorList)),
                                freeswitch_voicemail:unbind_outbound_pid(Parent),
                                {stop, normal, ended};
                            conference ->
                                send_conf_update(Call, ConfId, declined),
                                send_conf_update(Call, ConfId, ended),
                                {stop, normal, ended};
                            _ ->
                                send_update(Conn, UUID, ended, cpx_json_util:l2b(ErrorList)),
                                {next_state, StateName, StateName}
                        end
                end,
            {Reply, NextState};
        Error1 ->
            lager:info("Unhandled error ~p", [Error1]),
            {stop, normal}
    end,
    {Action, Event, State};

handle_info(call_hangup, _StateName, #state{type = conference, conf_id=ConfId,
    state_changes=StateChanges, orig_call=Call} = State) ->
    StateChanges1 = [{wrapup, os:timestamp()} | StateChanges],
    store_recording_info(State#state{state_changes=StateChanges1}),
    send_conf_update(Call, ConfId, ended),
    lager:debug("Received call_hangup", []),
    {stop, normal, State#state{state_changes=StateChanges1}};

handle_info(call_hangup, _StateName, #state{type = voicemail,
    state_changes=StateChanges} = State) ->
    StateChanges1 = [{wrapup, os:timestamp()} | StateChanges],
    store_recording_info(State#state{state_changes=StateChanges1}),
    end_call(State),
    lager:debug("Received call_hangup", []),
    {stop, normal, State#state{state_changes=StateChanges1}};

handle_info(call_hangup, _StateName, #state{state_changes=StateChanges} = State) ->
    StateChanges1 = [{wrapup, os:timestamp()} | StateChanges],
    store_recording_info(State#state{state_changes=StateChanges1}),
    case State#state.transferred of
        false ->
            case State#state.is_conference of
                false ->
                    end_call(State);
                true ->
                    ok
            end;
        _ ->
            #state{uuid=UUID, conn=Conn} = State,
            send_update(Conn, UUID, ended, hangup)
    end,
    lager:info("Received call_hangup, state ~p", [State]),
    {stop, normal, State#state{state_changes=StateChanges1}};

% handle_info({bgok, UUID, Msg}, _StateName, State) ->
%     lager:info("UUID: ~p", [UUID]),
%     {next_state, outbound_ringing, State};

handle_info({bgok, _MsgID, Reply}, StateName,
    #state{fnode=Fnode, uuid=UUID, bleg=BLeg} = State) ->
    ReplyList = string:tokens(Reply, " \n"),
    NextState = case ReplyList of
        ["+OK"|[UUID]] ->
            handle_call(Fnode, UUID),
            agent_ringing;
        ["+OK"|[BLeg]] ->
            handle_call(Fnode, BLeg),
            outbound_ringing;
        Reply1 ->
            lager:debug("Reply : ~p", [Reply1]),
            StateName
    end,
    {next_state, NextState, State};

handle_info({'DOWN',ParentMon,process,Parent,_Info}, _StateName,
        #state{parentmon = ParentMon, parent = Parent,
        state_changes=StateChanges} = State) ->
    StateChanges1 = [{wrapup, os:timestamp()} | StateChanges],
    store_recording_info(State#state{state_changes=StateChanges1}),
    end_call(State),
    lager:debug("Parent died", []),
    {stop, normal, State#state{state_changes=StateChanges1}};

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, #state{
        uuid=UUID, conn=Conn, agent_pid=AgentPid} = State) ->
    ARec = agent:dump_state(AgentPid),
    AvailChan = ARec#agent.available_channels,
    NewAvail = AvailChan ++ [voice],
    agent:set_avail(AgentPid, NewAvail),
    case State#state.is_conference of
        true ->
            send_update(Conn, UUID, ended, hangup);
        _ ->
            ok
    end,
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
process_voicemail(Fnode, Props) ->
    Type = proplists:get_value(type, Props),
    Agent = proplists:get_value(agent, Props),
    AgentPid = proplists:get_value(agent_pid, Props),
    Client = proplists:get_value(client, Props),
    Conn = proplists:get_value(conn, Props),
    Dest = proplists:get_value(destination, Props),
    UUID = proplists:get_value(uuid, Props),
    Call = proplists:get_value(call, Props),
    Parent = proplists:get_value(parent, Props),
    lager:debug("Parent value : ~p", [Parent]),
    ParentMon = erlang:monitor(process, Parent),
    CallerId = Agent,
    StateChanges = [{init, os:timestamp()}],
    case freeswitch:api(Fnode, create_uuid) of
        {ok, BLeg} ->
            originate(Fnode, BLeg, Dest, CallerId),
            send_update(Conn, UUID, outgoing_ringing),
            lager:debug("In call_destination state with bleg ~p", [BLeg]),
            StateChanges1 = [{ringing, os:timestamp()} | StateChanges],
            {ok, awaiting_destination,
                #state{
                    uuid = UUID, fnode = Fnode,
                    agent = Agent, agent_pid = AgentPid,
                    client = Client, orig_call = Call,
                    type = Type, destination = Dest,
                    bleg = BLeg, conn = Conn,
                    parent = Parent, parentmon = ParentMon,
                    state_changes = StateChanges1,
                    caller_id = {CallerId, Agent}}};
        _ -> {stop, uuid_not_created}
    end.

process_conference(Fnode, Props) ->
    Type = proplists:get_value(type, Props),
    Agent = proplists:get_value(agent, Props),
    AgentPid = proplists:get_value(agent_pid, Props),
    Conn = proplists:get_value(conn, Props),
    Dest = proplists:get_value(destination, Props),
    UUID = proplists:get_value(uuid, Props),
    Call = proplists:get_value(call, Props),
    OrigUUID = Call#call.original_id,
    CallerId = proplists:get_value(caller_id, Props),
    CallerNum = proplists:get_value(caller_number, Props, CallerId),
    ConfId = proplists:get_value(conf_id, Props),
    StateChanges = [{init, os:timestamp()}],
    conference_manager:register_leg(OrigUUID, ConfId, self(), {outband, Dest}, [{callback, ?MODULE}]),
    case freeswitch:api(Fnode, create_uuid) of
        {ok, BLeg} ->
            originate(Fnode, BLeg, Dest, CallerId, CallerNum),
            send_conf_update(Call, ConfId, ringing),
            lager:debug("In call_destination state with bleg ~p", [BLeg]),
            StateChanges1 = [{ringing, os:timestamp()} | StateChanges],
            {ok, awaiting_destination,
                #state{
                    uuid = UUID, fnode = Fnode,
                    agent = Agent, agent_pid = AgentPid,
                    type = Type, destination = Dest,
                    bleg = BLeg, conn = Conn,
                    conf_id = ConfId, orig_call = Call,
                    state_changes = StateChanges1,
                    caller_id = {CallerId, CallerNum}}};
        _ -> {stop, uuid_not_created}
    end.

process_outbound(Fnode, Props) ->
    Agent = proplists:get_value(agent, Props),
    AgentPid = proplists:get_value(agent_pid, Props),
    Conn = proplists:get_value(conn, Props),
    CallerId = "Reach",
    Client = proplists:get_value(client, Props),
    ClientRecord = case call_queue_config:get_client_by_name(Client) of
                     none -> undefined;
                     {ok, Res} -> Res
                   end,

    ClientOpts = case ClientRecord of
      undefined -> undefined;
      ClOpts -> ClOpts#client.options
    end,

    case freeswitch:api(Fnode, create_uuid) of
        {ok, UUID} ->
            originate(Fnode, UUID, Agent, CallerId),
            send_update(Conn, UUID, initiated),
            StateChanges = [{init, os:timestamp()}],
            {ok, precall, #state{uuid=UUID,
                    fnode=Fnode,
                    agent=Agent,
                    agent_pid=AgentPid,
                    client=ClientRecord,
                    client_opts=ClientOpts,
                    conn=Conn,
                    state_changes = StateChanges,
                    caller_id = {CallerId, Agent}}};
        _ -> {stop, uuid_not_created}
    end.

originate(Fnode, UUID, Agent, CallerId, CallerNum) ->
    OriginateCmd = "originate {origination_uuid=" ++ UUID ++
        ",bridge_early_media=true" ++
        ",origination_caller_id_name='" ++ CallerId ++
        "',origination_caller_id_number='" ++ CallerNum ++
        "',hangup_after_bridge=false}sofia/${domain}/" ++ Agent ++
        "@${domain} &park()",
    lager:info("Calling: expand ~s", [OriginateCmd]),
    freeswitch:bgapi(Fnode, expand, OriginateCmd).

originate(Fnode, UUID, Agent, CallerId) ->
    originate(Fnode, UUID, Agent, CallerId, CallerId).

originate_conference(Fnode, Agent, Conf) ->
    case freeswitch:api(Fnode, create_uuid) of
        {ok, UUID} ->
            OriginateCmd = "originate {origination_uuid=" ++ UUID ++
                ",bridge_early_media=true" ++
                ",origination_caller_id_name='Reach" ++
                "',origination_caller_id_number='Reach" ++
                "',hangup_after_bridge=false}sofia/${domain}/" ++ Agent ++
                "@${domain} &conference(" ++ Conf ++ ")",
            lager:info("Calling: expand ~s", [OriginateCmd]),
            freeswitch:bgapi(Fnode, expand, OriginateCmd);
        _ ->
            error
    end.

end_call(#state{fnode = Node, uuid = UUID, conn = Conn, bleg = BLeg} = _State) ->
    freeswitch:api(Node, uuid_kill, UUID),
    freeswitch:api(Node, uuid_kill, BLeg),
    send_update(Conn, UUID, ended, hangup).

drop_bleg(#state{fnode = Node, bleg = BLeg} = _State) ->
    freeswitch:api(Node, uuid_kill, BLeg).

case_event_name([UUID| EventPropList], StateName, State) ->
    % lager:debug("In case_event_name", []),
    Ename = case proplists:get_value("Event-Name", EventPropList) of
        "CUSTOM" -> {"CUSTOM", proplists:get_value("Event-Subclass", EventPropList)};
        Else -> Else
    end,
    lager:debug("Event ~p for ~p ", [Ename, UUID]),
    case_event_name(Ename, UUID, StateName, State).

case_event_name("CHANNEL_PARK", _UUID, StateName, State) ->
    {next_state, StateName, State};

case_event_name(_Other, _UUID, StateName, State) ->
    % lager:debug("In case_event_name/4", []),
    {next_state, StateName, State}.

store_recording_info(#state{bleg = Bleg, agent = Agent,
  orig_call = OrigCall, caller_id = CallerIdTuple, client = Client, state_changes = StateChanges} = _State) ->

  case Client of
    undefined ->
      lager:info("No Customer option was selected, skipping recording...", []),
      ok;
    _ ->
      case OrigCall of
        undefined ->
          case Bleg of
            undefined ->
              lager:warning("Undefined call id while storing recording info, skipping recording...", []),
              ok;
            _ ->
              Call = #call{id = Bleg,
                           client = Client,
                           callerid = CallerIdTuple,
                           queue = ?OUTBOUND_QUEUE,
                           source = self()},
              lager:info("Storing Call ~p with Call record ~p", [Bleg, lager:pr(Call, ?MODULE)]),
              cpx_recordings:store_file(Call, StateChanges, Agent)
          end;
        _ ->
          lager:info("Storing Call ~p with Call record ~p", [OrigCall#call.id, lager:pr(OrigCall, ?MODULE)]),
          cpx_recordings:store_file(OrigCall, StateChanges, Agent)
      end
  end.


send_conf_update(Call, ConfId, Status) ->
    CallId = Call#call.original_id,
    conference_manager:update_leg_state(CallId, ConfId, Status).

send_update(Conn, UUID, NextState) ->
    Time = util:now_ms(),
    ouc_update(Conn, ?EVENT_KEY, UUID,
    [{state, NextState}, {timestamp, Time}]).

send_update(Conn, UUID, NextState, Reason) ->
    Time = util:now_ms(),
    ouc_update(Conn, ?EVENT_KEY, UUID,
    [{state, NextState}, {reason, Reason}, {timestamp, Time}]).

ouc_update(Conn, Event, CallId, Data) ->
    Conn ! {Event, {l2b(CallId), Data}}.

handle_call(Fnode, UUID) ->
    Reply = freeswitch:handlecall(Fnode, UUID),
    lager:debug("handlecall reply for UUID ~p: ~p", [UUID, Reply]).

%% @hidden
handle_queue_transfer({_QName, _Qpid}, _StateName, Call, _GenMediaState, #state{fnode = Fnode} = State) ->
    case State#state.moh of
        none ->
            ok;
        _ ->
            freeswitch:bgapi(Fnode, uuid_broadcast, Call#call.id ++ " local_stream://" ++ State#state.moh)
    end,
    freeswitch:api(Fnode, uuid_park, Call#call.id),
    {ok, State}.

handle_conference_to_queue({LegId, Request}, Call, Queue, _Opts, _GenMediaState,
        #state{fnode=Cnode, uuid=CallId, bleg=RingUUID} = State)
        when is_list(RingUUID) ->

    #call{priority=Priority, client=Client, dnis=Dnis, url_vars=UrlVars} = Call,
    ClientId = Client#client.id,

    Info = [{dnis, Dnis}, {url_vars, UrlVars}],

    lager:info("Converting ~p to a conference", [CallId]),
    freeswitch:api(Cnode, uuid_setvar, CallId ++ " hangup_after_bridge false"),
    freeswitch:api(Cnode, uuid_setvar, RingUUID ++ " hangup_after_bridge false"),

    freeswitch_media_manager:new_conference({LegId, Request}, Call, RingUUID, Queue, Priority,
        ClientId, Info),
    {ok, State}.
