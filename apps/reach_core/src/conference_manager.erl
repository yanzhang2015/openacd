-module(conference_manager).
-include("agent.hrl").
-include("call.hrl").
-include("gen_media.hrl").

-export([
	start/4,

	register_leg/4,
	register_leg/5,
	remove_leg/2,
	transfer_leg/2,

	update_leg_state/3,

	hold_leg/3,
	unhold_leg/3,

	get_legs/1
]).

-type(leg_id() :: string()).
-type(leg_fs_uuid() :: string()).

-record(st, {
	call_id :: string(),
	original_media :: pid(),
	original_agent :: string(),
	initiated_legs :: dict(),
	accepted_legs :: dict(),
	freeswitch_node :: atom() | undefined,
	moh = "moh" :: string() | 'none',
	hold_leg :: undefined | {hold, leg_id(), leg_fs_uuid()},
	in_consultation = false :: boolean(),
	pending_transfer = false :: boolean()
}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

start(CallId, OrigMedia, OrigAgent, Opts) ->
	gen_server:start({via, gproc, {n, g, {?MODULE, CallId}}},
		?MODULE, [CallId, OrigMedia, OrigAgent, Opts], []).

register_leg(CallId, LegId, LegPid, RequestType) ->
	register_leg(CallId, LegId, LegPid, RequestType, []).

register_leg(CallId, LegId, LegPid, RequestType, Opts) ->
	gen_server:cast(server_for(CallId), {register_leg, LegId, LegPid,
		RequestType, Opts}).

% Update leg state to ringing, accepted, declined, ended
update_leg_state(CallId, LegId, State) ->
	gen_server:cast(server_for(CallId), {update_leg_state, LegId, State}).

remove_leg(CallId, LegId) ->
	gen_server:cast(server_for(CallId), {remove_leg, LegId}).

transfer_leg(CallId, TransferLegId) ->
	gen_server:cast(server_for(CallId), {transfer_leg, TransferLegId}).

hold_leg(CallId, LegId, LegCallUuid) ->
	gen_server:cast(server_for(CallId), {hold_leg, LegId, LegCallUuid}).

unhold_leg(CallId, LegId, LegCallUuid) ->
	gen_server:cast(server_for(CallId), {unhold_leg, LegId, LegCallUuid}).

get_legs(CallId) ->
	gen_server:call(server_for(CallId), get_legs).

%% gen_server callbacks

init([CallId, OrigMedia, OrigAgent, Opts]) ->
	lager:info("Starting conference_manager with args ~p", [[CallId, OrigMedia, OrigAgent, Opts]]),

	%% for starting/stopping moh, to be handled by gen_media callbacks later on
	FSNode = proplists:get_value(fs_node, Opts),
	InitHold = proplists:get_value(hold, Opts, false),
	OrigLegUuid = proplists:get_value(fs_uuid, Opts),

	{HoldLeg, State} = case InitHold of
		true ->
			lager:info("Conference initiated while on hold, playing moh"),
			play_moh(FSNode, CallId, "moh"),
			{{hold, "original", OrigLegUuid}, onhold};
		_ ->
			{undefined, accepted}
	end,

	LegProp = init_leg_prop(CallId, "original", OrigMedia,
		{agent, OrigAgent}, [{state, State}, {fs_uuid, OrigLegUuid}]),
	Accepted = dict:store("original", LegProp, dict:new()),
	St = #st{
		call_id = CallId,
		original_media = OrigMedia,
		original_agent = OrigAgent,
		initiated_legs = dict:new(),
		accepted_legs = Accepted,
		freeswitch_node = FSNode,
		hold_leg = HoldLeg
	},

	gproc:reg({p, g, {channel_destroy_event, CallId}}),
	{ok, St}.

handle_call(get_legs, _From, #st{initiated_legs=Initiated, accepted_legs=Accepted} = St) ->
	Legs = {dict:to_list(Initiated), dict:to_list(Accepted)},
	{reply, Legs, St};

handle_call(_Request, _From, St) ->
	Reply = ok,
	{reply, Reply, St}.

handle_cast({register_leg, LegId, LegPid, RequestType, Opts}, St) ->
	lager:info("Registering new leg with args: ~p", [[LegId, LegPid, RequestType, Opts]]),
	#st{call_id = CallId, initiated_legs = Initiated, accepted_legs = Accepted} = St,

	LegProp = init_leg_prop(CallId, LegId, LegPid, RequestType, Opts),
	send_leg_state_update(LegProp, Accepted, initiated),
	Initiated2 = dict:store(LegId, LegProp, Initiated),

	{noreply, St#st{initiated_legs = Initiated2}};

handle_cast({remove_leg, LegId}, St) ->
	#st{initiated_legs = Initiated, accepted_legs = Accepted} = St,

	case dict:is_key(LegId, Initiated) of
		true ->
			LegProp = dict:fetch(LegId, Initiated),
			stop_leg(LegProp);
		_ ->
			case dict:is_key(LegId, Accepted) of
				true ->
					LegProp = dict:fetch(LegId, Accepted),
					stop_leg(LegProp);
				_ ->
					ok
			end
	end,

	{noreply, St};

handle_cast({update_leg_state, LegId, ringing}, St) ->
	handle_cast({update_leg_state, LegId, {ringing, []}}, St);

handle_cast({update_leg_state, LegId, {ringing, Info}}, St) ->
	#st{initiated_legs = Initiated, accepted_legs = Accepted} = St,

	{Initiated2, LegProp2} = update_leg_in_dict(LegId, [{state, ringing} | Info], Initiated),
	send_leg_state_update(LegProp2, Accepted, ringing),

	{noreply, St#st{initiated_legs = Initiated2}};

handle_cast({update_leg_state, LegId, accepted}, St) ->
	#st{call_id = _CallId, initiated_legs = Initiated, accepted_legs = Accepted,
		freeswitch_node = FSNode, in_consultation = InConsultation, hold_leg = HoldLeg} = St,

	% Move leg from initiated to accepted dict
	LegProp = dict:fetch(LegId, Initiated),
	LegProp2 = update_leg_prop_info({state, accepted}, LegProp),
	Initiated2 = dict:erase(LegId, Initiated),
	Accepted2 = dict:store(LegId, LegProp2, Accepted),

	% Send conference updates
	NewLegPid = LegProp2#conference_leg.pid,
	ExistingLegs = dict:to_list(Accepted),
	lists:foreach(fun({_, ExistingLeg}) ->
		% Simulate conference update regarding existing leg to newly accepted leg
		NewLegPid ! {cpx_conference_update, {ExistingLeg, ExistingLeg#conference_leg.state}},

		% Send update regarding newly accepted leg to existing leg
		Pid = ExistingLeg#conference_leg.pid,
		Pid ! {cpx_conference_update, {LegProp2, accepted}}
	end, ExistingLegs),

	InitiatedLegs = dict:to_list(Initiated2),
	lists:foreach(fun({_, Leg}) ->
		% Simulate conference update regarding initiated leg to newly accepted leg
		NewLegPid ! {cpx_conference_update, {Leg, Leg#conference_leg.state}}
	end, InitiatedLegs),

	InConsultation2 = case {InConsultation, HoldLeg} of
		{false, {hold, HoldLegId, HoldLegCallUuid}} ->
			lager:info("Starting conference consultation"),
			% assume moh is already being played, just unhold hold_leg
			HoldLegProp = dict:fetch(HoldLegId, Accepted2),
			HoldLegPid = HoldLegProp#conference_leg.pid,
			gen_media:hold_update(HoldLegPid, conference_onhold),
			unhold_call_id(FSNode, HoldLegCallUuid),
			true;
		_ ->
			InConsultation
	end,

	{noreply, St#st{initiated_legs = Initiated2, accepted_legs = Accepted2,
		in_consultation = InConsultation2, pending_transfer = false}};

handle_cast({update_leg_state, LegId, declined}, St) ->
	#st{initiated_legs = Initiated, accepted_legs = Accepted} = St,

	LegProp = dict:fetch(LegId, Initiated),
	send_leg_state_update(LegProp, Accepted, declined),

	{noreply, St};

handle_cast({update_leg_state, LegId, ended}, St) ->
	lager:info("Leg ~p ended", [LegId]),
	#st{initiated_legs = Initiated, accepted_legs = Accepted,
		call_id = CallId, freeswitch_node = FSNode,
		hold_leg = HoldLeg, moh = Moh, in_consultation = InConsultation,
		pending_transfer = PendingTransfer} = St,

	LegType = case dict:is_key(LegId, Initiated) of
		true ->
			initiated;
		_ ->
			case dict:is_key(LegId, Accepted) of
				true ->
					accepted;
				_ ->
					none
			end
	end,

	case LegType of
		initiated ->
			% Remove leg from initiated dict
			{Initiated2, LegProp} = remove_leg_from_dict(LegId, Initiated),
			send_leg_state_update(LegProp, Accepted, ended),

			AcceptedCount = dict:size(Accepted),
			InitiatedCount = dict:size(Initiated2),
			case {AcceptedCount, InitiatedCount} of
				{0, 0} ->
					freeswitch:api(FSNode, uuid_kill, CallId),
					lager:info("No legs left in conference, terminating"),
					{stop, normal, St};
				_ ->
					{noreply, St#st{initiated_legs = Initiated2}}
			end;
		accepted ->
			% Remove leg from accepted dict
			{Accepted2, LegProp} = remove_leg_from_dict(LegId, Accepted),
			send_leg_state_update(LegProp, Accepted2, ended),

			AcceptedCount = dict:size(Accepted2),
			InitiatedCount = dict:size(Initiated),

			case {AcceptedCount, InitiatedCount, PendingTransfer} of
				{0, 0, false} ->
					freeswitch:api(FSNode, uuid_kill, CallId),
					lager:info("No legs left in conference, terminating"),
					{stop, normal, St};
				{1, _, _} ->
					HoldLeg2 = case dict:to_list(Accepted2) of
						[{LastLegId, #conference_leg{pid = LastLegPid, state = onhold, fs_uuid = LastLegCallUuid}}] ->
							lager:info("Only agent left is onhold, converting mute to hold"),
							unmute_call_id(FSNode, CallId, LastLegCallUuid),
							hold_call_id(FSNode, LastLegCallUuid),

							case InConsultation of
								true ->
									lager:info("Ending conference consultation"),
									gen_media:hold_update(LastLegPid, conference_offhold);
								_ ->
									play_moh(FSNode, CallId, Moh)
							end,
							gen_media:hold_update(LastLegPid, hold),
							{hold, LastLegId, LastLegCallUuid};
						_ ->
							case InConsultation of
								true ->
									stop_moh(FSNode, CallId);
								_ ->
									ok
							end,
							undefined
					end,
					{noreply, St#st{accepted_legs = Accepted2, in_consultation = false, hold_leg = HoldLeg2}};
				_ ->
					case HoldLeg of
						{hold, LegId, _} ->
							case InConsultation of
								true ->
									lager:info("Ending conference consultation"),
									LegProp = dict:fetch(LegId, Accepted2),
									LegPid = LegProp#conference_leg.pid,
									gen_media:hold_update(LegPid, conference_offhold);
								_ ->
									ok
							end,
							stop_moh(FSNode, CallId),
							{noreply, St#st{accepted_legs = Accepted2, in_consultation = false, hold_leg = undefined}};
						_ ->
							{noreply, St#st{accepted_legs = Accepted2}}
					end
			end;
		_ ->
			{noreply, St}
	end;

handle_cast({hold_leg, LegId, LegCallUuid}, #st{
		call_id=ConfUuid,
		accepted_legs=Accepted,
		freeswitch_node=FSNode,
		hold_leg=HoldLeg,
		moh=Moh} = St) ->

	AcceptedCount = dict:size(Accepted),
	HoldLeg2 = case AcceptedCount =:= 1 of
		true ->
			lager:info("Only agent left is being put onhold"),
			hold_call_id(FSNode, LegCallUuid),
			play_moh(FSNode, ConfUuid, Moh),
			{hold, LegId, LegCallUuid};
		_ ->
			mute_call_id(FSNode, ConfUuid, LegCallUuid),
			HoldLeg
	end,

	{Accepted2, LegProp2} = update_leg_in_dict(LegId, {state, onhold}, Accepted),
	send_leg_state_update(LegProp2, Accepted2, onhold),
	{noreply, St#st{hold_leg = HoldLeg2, accepted_legs = Accepted2}};

handle_cast({unhold_leg, LegId, LegCallUuid}, #st{
		call_id=ConfUuid,
		accepted_legs=Accepted,
		freeswitch_node=FSNode,
		in_consultation=InConsultation,
		hold_leg=HoldLeg} = St) ->

	{InConsultation2, HoldLeg2} = case HoldLeg of
		{hold, LegId, LegCallUuid} ->
			case InConsultation of
				true ->
					lager:info("Ending conference consultation"),
					LegProp = dict:fetch(LegId, Accepted),
					LegPid = LegProp#conference_leg.pid,
					gen_media:hold_update(LegPid, conference_offhold),
					stop_moh(FSNode, ConfUuid);
				_ ->
					stop_moh(FSNode, ConfUuid),
					unhold_call_id(FSNode, LegCallUuid)
			end,
			{false, undefined};
		_ ->
			unmute_call_id(FSNode, ConfUuid, LegCallUuid),
			{InConsultation, HoldLeg}
	end,
	{Accepted2, LegProp2} = update_leg_in_dict(LegId, {state, accepted}, Accepted),
	send_leg_state_update(LegProp2, Accepted2, offhold),
	{noreply, St#st{accepted_legs = Accepted2, hold_leg = HoldLeg2, in_consultation = InConsultation2}};

handle_cast({transfer_leg, _TransferLegId}, St) ->
	{noreply, St#st{pending_transfer = true}};

handle_cast(_, St) ->
	{noreply, St}.

handle_info({channel_destroy_event, CallId}, #st{
		call_id=CallId,
		initiated_legs=Initiated,
		accepted_legs=Accepted} = St) ->
	lager:info("Caller leg ended, stopping all legs for ~p", [CallId]),
	stop_all_legs(Initiated, Accepted),
	{stop, normal, St};
handle_info(_Info, St) ->
	{noreply, St}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, St, _Extra) ->
	{ok, St}.

%% Internal functions

server_for(Pid) when is_pid(Pid) ->
	Pid;
server_for(CallId) ->
	{via, gproc, {n, g, {?MODULE, CallId}}}.

init_leg_prop(CallId, LegId, LegPid, RequestType, Opts) ->
	Agent = case RequestType of
		{agent, AgentLogin} ->
			get_agent(AgentLogin);
		_ ->
			undefined
	end,
	Prop = #conference_leg{
		pid = LegPid,
		id = LegId,
		original_id = CallId,
		request = RequestType,
		agent = Agent
	},
	update_leg_prop_info(Opts, Prop).

update_leg_prop_info({Key, Val}, Prop) ->
	update_leg_prop_info([{Key, Val}], Prop);
update_leg_prop_info([], Prop) ->
	Prop;
update_leg_prop_info([{state, State} | Fields], Prop) ->
	update_leg_prop_info(Fields, Prop#conference_leg{state = State});
update_leg_prop_info([{agent, AgentLogin} | Fields], Prop) ->
	Agent = get_agent(AgentLogin),
	update_leg_prop_info(Fields, Prop#conference_leg{agent = Agent});
update_leg_prop_info([{fs_uuid, FsUuid} | Fields], Prop) ->
	update_leg_prop_info(Fields, Prop#conference_leg{fs_uuid = FsUuid});
update_leg_prop_info([{callback, Callback} | Fields], Prop) ->
	update_leg_prop_info(Fields, Prop#conference_leg{callback = Callback});
update_leg_prop_info([_|Fields], Prop) ->
	update_leg_prop_info(Fields, Prop).

send_leg_state_update(NewLegProp, Accepted, State) ->
	LegsToUpdate = dict:to_list(Accepted),
	lists:foreach(fun({_, LegProp}) ->
		Pid = LegProp#conference_leg.pid,
		Pid ! {cpx_conference_update, {NewLegProp, State}}
	end, LegsToUpdate).

get_agent(AgentLogin) ->
	case agent_auth:get_agent_by_login(AgentLogin) of
		{ok, AgentAuth} ->
			FName = case AgentAuth#agent_auth.firstname of
				"" -> undefined;
				F -> F
			end,
			LName = case AgentAuth#agent_auth.lastname of
				"" -> undefined;
				L -> L
			end,
			{AgentLogin, FName, LName};
		_ ->
			undefined
	end.

stop_leg(LegProp) ->
	case LegProp of
		#conference_leg{callback = Callback, pid = LegPid, request = {outband, _}}
				when Callback =/= undefined, LegPid =/= self() ->
			lager:info("Calling ~p:stop for ~p", [Callback, LegPid]),
			catch Callback:stop(LegPid);
		#conference_leg{pid = LegPid} when is_pid(LegPid), LegPid =/= self() ->
			lager:info("Calling gen_media:leave_conference for ~p", [LegPid]),
			gen_media:leave_conference(LegPid);
		_ ->
			ok
	end.

stop_all_legs(Initiated, Accepted) ->
	lists:foreach(fun({_, Prop}) ->
		stop_leg(Prop)
	end, dict:to_list(Initiated)),
	lists:foreach(fun({_, Prop}) ->
		stop_leg(Prop)
	end, dict:to_list(Accepted)).

play_moh(FSNode, CallId, Moh) ->
	lager:info("Playing moh"),
	freeswitch:api(FSNode, uuid_broadcast, CallId ++ " local_stream://"
		++ Moh ++ " aleg").

stop_moh(FSNode, CallId) ->
	lager:info("Stopping moh"),
	%% WARNING will not work if broadcast was played twice for some reason
	%% TODO look for more reliable way to stop moh
	freeswitch:api(FSNode, uuid_break, CallId).

get_member_by_call_id(FSNode, Conference, CallId) ->
	% can't find a freeswitch api for this
	{ok, ConfList} = freeswitch:api(FSNode, conference, Conference ++ " list"),
		case string:str(ConfList, "not found") > 0 of
			true ->
				[];
			_ ->
				Members = string:tokens(ConfList, "\n"),
				find_member(Members, CallId)
	end.

find_member([], _) ->
	[];
find_member([Member | Rest], CallId) ->
	MemberVals = string:tokens(Member, ";"),
	case lists:nth(3, MemberVals) =:= CallId of
		true ->
			lists:nth(1, MemberVals);
		_ ->
			find_member(Rest, CallId)
	end.

mute_call_id(FSNode, Conference, CallId) ->
	case get_member_by_call_id(FSNode, Conference, CallId) of
		[] ->
			ok;
		MemberId ->
			lager:info("Muting ~p/member ~p", [CallId, MemberId]),
			freeswitch:api(FSNode, conference, " " ++ Conference ++ " mute " ++ MemberId),
			freeswitch:api(FSNode, conference,  " " ++ Conference ++ " deaf " ++ MemberId)
	end.

unmute_call_id(FSNode, Conference, CallId) ->
	case get_member_by_call_id(FSNode, Conference, CallId) of
		[] ->
			ok;
		MemberId ->
			lager:info("Unmuting ~p/member ~p", [CallId, MemberId]),
			freeswitch:api(FSNode, conference,  " " ++ Conference ++ " unmute " ++ MemberId),
			freeswitch:api(FSNode, conference,  " " ++ Conference ++ " undeaf " ++ MemberId)
	end.

hold_call_id(FSNode, CallId) ->
	lager:info("Holding ~p", [CallId]),
	freeswitch:api(FSNode, uuid_phone_event, CallId ++ " hold").

unhold_call_id(FSNode, CallId) ->
	lager:info("Unholding ~p", [CallId]),
	freeswitch:api(FSNode, uuid_phone_event, CallId ++ " talk").

update_leg_in_dict(LegId, Updates, Dict) ->
	LegProp = dict:fetch(LegId, Dict),
	LegProp2 = update_leg_prop_info(Updates, LegProp),
	Dict2 = dict:update(LegId, fun(_) ->
		LegProp2
	end, Dict),
	{Dict2, LegProp2}.

remove_leg_from_dict(LegId, Dict) ->
	case dict:is_key(LegId, Dict) of
		true ->
			LegProp = dict:fetch(LegId, Dict),
			Dict2 = dict:erase(LegId, Dict),
			{Dict2, LegProp};
		_ ->
			false
	end.
