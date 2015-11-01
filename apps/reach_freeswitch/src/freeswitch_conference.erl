-module(freeswitch_conference).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("reach_core/include/queue.hrl").
-include_lib("reach_core/include/call.hrl").
-include_lib("reach_core/include/agent.hrl").
-include_lib("reach_core/include/gen_media.hrl").

-behaviour(gen_media).

-export([
	start/8,
	init/1,
	prepare_endpoint/2,
	get_monitor_leg/1,

	handle_ring/4,
	handle_ringing/5,
	handle_ring_stop/4,
	handle_answer/5,
	handle_wrapup/5,
	handle_call/6,
	handle_cast/5,
	handle_info/5,
	handle_hold/2,
	handle_unhold/2,
	handle_hold_update/3,
	terminate/5,
	code_change/6,

	handle_conference_to_queue/6,
	handle_conference_to_outband/7,
	handle_leave_conference/3,
	handle_queue_transfer/5,
	handle_transfer_outband/5
]).

-record(state, {
	leg_id :: term(),
	queue :: string() | undefined,
	queue_pid :: pid() | undefined,
	cnode :: atom(),
	manager_pid :: pid() | undefined,

	uuid :: string() | undefined,
	original_uuid :: string() | undefined,
	original_bleg :: string() | undefined,
	original_media :: pid(),
	agent_login :: undefined | string(),

	ringchannel :: pid() | undefined,
	ringuuid :: string() | undefined,

	hold :: undefined | hold,
	moh = "moh" :: string() | 'none'
}).

-spec(start/8 :: ({LegId :: string(), Request :: tuple()}, Cnode :: atom(), Call :: #call{}, RingUuid :: string(), Queue :: string(), Priority :: pos_integer(), Client :: #client{} | string(), Info :: list()) -> {'ok', pid()}).
start({LegId, Request}, Cnode, Call, RingUuid, Queue, Priority, Client, Info) ->
	gen_media:start(?MODULE, [{LegId, Request}, Cnode, Call, RingUuid, Queue, Priority, Client, Info]).

init([{LegId, Request}, Cnode, Call, RingUuid, Queue, Priority, Client, Info]) ->
	{ok, Uuid} = freeswitch:api(Cnode, create_uuid),

	process_flag(trap_exit, true),
	Manager = whereis(freeswitch_media_manager),
	Dnis = proplists:get_value(dnis, Info),

	OrigUuid = Call#call.original_id,
	CallSegment = Call#call.call_segment,
	Skills = Call#call.skills,
	CallerId = Call#call.callerid,
	OrigMedia = Call#call.source,
	UrlVars = Call#call.url_vars,
	PsInfo = [{conference, true}],

	Ps = [
		{id, Uuid},
		{original_id, OrigUuid},
		{type, voice},
		{media_type, conference},
		{priority, 20},
		{client, Client},
		{media_path, inband},
		{queue, Queue},
		{dnis, Dnis},
		{caller_id, CallerId},
		{priority, Priority},
		{skills, Skills},
		{call_segment, CallSegment},
		{info, PsInfo},
		{url_vars, UrlVars}
	],

	lager:info("Starting freeswitch_conference with details ~p", [Ps]),
	gproc:reg({p, g, {freeswitch_node, Uuid}}, Cnode),

	conference_manager:register_leg(OrigUuid, LegId, self(), Request),
	{ok, #state{leg_id=LegId, cnode=Cnode, manager_pid=Manager, queue=Queue,
			uuid=Uuid, original_uuid = OrigUuid, original_bleg = RingUuid,
			original_media = OrigMedia},
	Ps}.

prepare_endpoint(Agent,Options) ->
	freeswitch_media:prepare_endpoint(Agent, Options).

get_monitor_leg(#state{ringuuid = RingUuid} = _State) ->
	RingUuid.

handle_ring(_,_,_,State) ->
	{ok, State}.

handle_ringing(_,_,Agent,GenMediaState,State) ->
	LegId = State#state.leg_id,
	OrigUuid = State#state.original_uuid,

	{Agent, RingUuid, RingPid} = case GenMediaState of
		#inqueue_ringing_state{outband_ring_pid = undefined} ->
			{undefined, "", undefined};
		#inqueue_ringing_state{outband_ring_pid = P, ring_pid = {Agt, _Apid}} ->
			{Agt, freeswitch_ring:get_uuid(P), P};
		#oncall_ringing_state{outband_ring_pid = undefined} ->
			{undefined, "", undefined};
		#oncall_ringing_state{outband_ring_pid = P, ring_pid = {Agt, _Apid}} ->
			{Agt, freeswitch_ring:get_uuid(P), P}
	end,

	conference_manager:update_leg_state(OrigUuid, LegId, {ringing, [{agent, Agent}, {fs_uuid, RingUuid}]}),
	{ok, State#state{ringuuid = RingUuid, ringchannel = RingPid,
		agent_login = Agent}}.

handle_ring_stop(_,_,_,State) ->
	LegId = State#state.leg_id,
	OrigUuid = State#state.original_uuid,

	conference_manager:update_leg_state(OrigUuid, LegId, declined),
	{ok, State#state{ringuuid = undefined, ringchannel = undefined,
		agent_login = undefined}}.

handle_answer(_,_,_,_,State) ->
	Cnode = State#state.cnode,
	LegId = State#state.leg_id,
	OrigUuid = State#state.original_uuid,
	RingUuid = State#state.ringuuid,

	lager:info("Transferring to ~p conference on node ~p", [OrigUuid, Cnode]),
	freeswitch:bgapi(Cnode, uuid_transfer, RingUuid ++ " conference:" ++
		OrigUuid ++ "@reach inline"),

	conference_manager:update_leg_state(OrigUuid, LegId, accepted),
	{ok, State}.

handle_leave_conference({_OrigMedia,_},_GenMediaState,State) ->
	lager:info("Telling agent to leave the conference"),
	{hangup, State};
handle_leave_conference(_,_,State) ->
	{error, invalid, State}.

handle_wrapup(_, _StateName, #call{media_path = inband}, _GenMediaState, State) ->
	{hangup, State};
handle_wrapup(_From, _StateName, _Call, _GenMediaState, State) ->
	{error, invalid, State}.

handle_conference_to_queue({LegId, Request}, Call, Queue, Opts, _GenMediaState,
		#state{ringuuid = RingUuid} = State) when is_list(RingUuid) ->

	#call{original_id=OrigId, priority=Priority, client=Client, dnis=Dnis} = Call,
	ClientId = Client#client.id,
	Info = [{dnis, Dnis}],

	case proplists:get_value(transfer, Opts) of
		true ->
			conference_manager:transfer_leg(OrigId, LegId);
		_ ->
			ok
	end,

	freeswitch_media_manager:new_conference({LegId, Request}, Call, RingUuid, Queue,
		Priority, ClientId, Info),

	{ok, State}.

handle_conference_to_outband(Call, Dest, AgentRec, LegId, Opts, _GenMediaState,
		#state{cnode = Cnode, ringuuid = RingUUID} = State) when is_list(RingUUID) ->

	#call{original_id=OrigId, callerid={CallerId, CallerNum}} = Call,
	#agent{login=Agent, source=AgentPid, connection=Conn} = AgentRec,

	Props = [
		{agent, Agent},
		{agent_pid, AgentPid},
		{conn, Conn},
		{destination, Dest},
		{uuid, OrigId},
		{type, conference},
		{call, Call},
		{caller_id, CallerId},
		{caller_number, CallerNum},
		{conf_id, LegId}
	],

	case proplists:get_value(transfer, Opts) of
		true ->
			conference_manager:transfer_leg(OrigId, LegId);
		_ ->
			ok
	end,

	Reply = freeswitch_outbound:start_link(Cnode, Props),
	case Reply of
		{ok, OutboundPid} ->
			{{ok, OutboundPid}, State};
		_ ->
			{error, State}
	end.

handle_queue_transfer(_,_,_,_,#state{ringchannel = Channel} = State)
		when is_pid(Channel) ->
	freeswitch_ring:hangup(Channel),
	{ok, State#state{ringchannel = undefined}};
handle_queue_transfer(_,_,_,_,State) ->
	{ok, State}.

handle_transfer_outband(Destination0, StateName, Call, _GenMediaState, State) when StateName =:= oncall ->
	Destination = freeswitch_media_manager:fix_sip_addr(Destination0),
	Fnode = State#state.cnode,
	UUID = State#state.original_uuid,
	Client = Call#call.client,
	ClientOpts = Client#client.options,

	{CallerNameOpt, CallerNumberOpt} = case proplists:get_value(<<"caller_id">>, ClientOpts) of
		undefined -> {"", ""};
		{BinName, BinNumber} when is_binary(BinName), is_binary(BinNumber) ->
			{binary_to_list(BinName), binary_to_list(BinNumber)};
		CidOut -> CidOut
	end,

	BaseDS = freeswitch_media_manager:get_default_dial_string(),
	RingOpts = [CallerNameOpt, CallerNumberOpt, "hangup_after_bridge=true"],
	Dialstring = freeswitch_media_manager:do_dial_string_without_sip_opts(BaseDS, Destination, RingOpts),
	freeswitch:bgapi(Fnode, uuid_setvar, UUID ++ " transfer_ringback local_stream://" ++ State#state.moh),
	freeswitch:bgapi(Fnode, uuid_transfer, UUID ++ " 'm:^:bridge:" ++ Dialstring ++ "' inline"),
	{ok, State};

handle_transfer_outband(_,_,_,_,State) ->
	{ok, State}.

handle_call(_,_,_,_,_,State) ->
	{ok, State}.
handle_cast(_,_,_,_,State) ->
	{ok, State}.

handle_info(Info,_,_,_,State) ->
	lager:debug("Got info: ~p", [Info]),
	{noreply, State}.

handle_hold(GenMediaState, #state{ringuuid=RingUuid, original_uuid=OrigUuid,
		leg_id=LegId} = State) when is_list(RingUuid) ->

	lager:debug("Holding conference leg"),
	conference_manager:hold_leg(OrigUuid, LegId, RingUuid),

	handle_hold_update(hold, GenMediaState, State).

handle_unhold(GenMediaState, #state{ringuuid=RingUuid, original_uuid=OrigUuid,
		leg_id=LegId} = State) when is_list(RingUuid) ->

	lager:debug("Unholding conference leg"),
	conference_manager:unhold_leg(OrigUuid, LegId, RingUuid),

	handle_hold_update(undefined, GenMediaState, State).

handle_hold_update(HoldStatus, _GenMediaState, #state{hold = Hold} = State) ->
	Hold2 = case HoldStatus of
		hold -> hold;
		undefined -> undefined;
		_ -> Hold
	end,
	{ok, State#state{hold = Hold2}}.

terminate(_Reason, StateName, _Call, _Extra, State) ->
	handle_terminate(StateName, State).

code_change(_,_,_,_,_,State) ->
	{ok, State}.

%% Internal functions

handle_terminate(StateName, State) when StateName =:= oncall orelse
		StateName =:= inqueue_ringing ->

	LegId = State#state.leg_id,
	OrigUuid = State#state.original_uuid,
	RingUuid = State#state.ringuuid,
	freeswitch:api(State#state.cnode, uuid_kill, RingUuid),

	conference_manager:update_leg_state(OrigUuid, LegId, ended),
	ok;
handle_terminate(StateName, State) when StateName =:= inqueue ->
	LegId = State#state.leg_id,
	OrigUuid = State#state.original_uuid,

	conference_manager:update_leg_state(OrigUuid, LegId, ended),
	ok;
handle_terminate(_, _) ->
	ok.
