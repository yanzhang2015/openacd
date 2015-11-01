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

%% @doc The gen_media callback module for voice calls through freeswitch.
%% @see freeswitch_media_manager

-module(freeswitch_media).
-author("Micah").

-behaviour(gen_media).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("reach_core/include/queue.hrl").
-include_lib("reach_core/include/call.hrl").
-include_lib("reach_core/include/agent.hrl").
-include("cpx_freeswitch_pb.hrl").
%-include_lib("reach_core/include/gen_media.hrl").

-define(TIMEOUT, 10000).

-define(DEFAULT_PRIORITY, 10).
-define(DEFAULT_VM_PRIORITY_DIFF, 10).
-define(ORIGINAL_LEG_ID, "original").
-define(MORATORIUM_TIMEOUT, 32000).

%% API
-export([
	start/3,
	start/4,
	start_link/3,
	get_call/1,
	%get_queue/1,
	%get_agent/1,
	%unqueue/1,
	%set_agent/3,
	dump_state/1,
	'3rd_party_pickup'/1
	]).

%% gen_media callbacks
-export([
	init/1,
	urlpop_getvars/1,
	get_monitor_leg/1,
	prepare_endpoint/2,
	handle_ring/4,
	handle_ring_stop/4,
	handle_answer/5,
	handle_voicemail/4,
	handle_voicemail_outbound/6,
	handle_transfer_outband/5,
	handle_spy/5,
	handle_announce/5,
%% TODO added for testing only (implemented with focus on real Calls - no other media)
	handle_end_call/4,
	handle_agent_transfer/4,
	handle_queue_transfer/5,
	handle_conference_to_queue/6,
	handle_conference_to_outband/7,
	handle_leave_conference/3,
	handle_drop_external/3,
	handle_wrapup/5,
	handle_call/6,
	handle_cast/5,
	handle_info/5,
	handle_hold/2,
	handle_unhold/2,
	handle_hold_update/3,
	handle_transfer_to_node/3,
	terminate/5,
	code_change/6]).

%% optional gen_media callbacks
-export([
	is_recording_enabled/0,
	handle_recover/3,
	handle_run_ivr/4
]).

-type(internal_statename() ::
	'inivr' | % never been in queue.
	'inqueue' | % waiting for an agent to call
	'inqueue_ringing' | % got an agent, waiting for the anser
	'oncall' | % oncall with an agent.
	'oncall_ringing' | % starting an agent transfer
	'oncall_hold' | % simple hold
	'hold_conference' | % has a conference, but agent (self) not a member
	'hold_conference_3rdparty' | % a conference is up, as is a call to a
		% 3rd party, but agent is member of neither
	'in_conference_3rdparty' | % agent is a member of the conference,
		% but there is a 3rd party in limbo.
	'3rd_party' | % there is a conference, but agent is talking w/ 3rd party.
	'in_conference' | % agent is a member of the conference, no limbo party
	'wrapup_conference' | % there is no agent
	'blind_transfered'
).

-define(cdr_states, ['oncall', 'oncall_hold', 'hold_conference',
	'hold_conference_3rdparty', 'in_conference_3rdparty', '3rd_party',
	'in_conference', 'wrapup_conference', 'blind_transfered']).

-record(state, {
	statename :: internal_statename(),
	uuid :: string(),
	cook :: pid() | 'undefined',
	queue :: string() | 'undefined',
	cnode :: atom(),
	dialstring :: string(),
	agent :: string() | 'undefined',
	initial_agent :: string() | 'undefined',
	agent_pid :: pid() | 'undefined',
	ringchannel :: pid() | 'undefined',
	ringuuid :: string() | 'undefined',
	manager_pid :: 'undefined' | any(),
	voicemail = false :: 'false' | string(),
	xferchannel :: pid() | 'undefined',
	xferuuid :: string() | 'undefined',
	in_control = false :: boolean(),
	queued = false :: boolean(),
	allow_voicemail = false :: boolean(),
	vm_priority_diff = ?DEFAULT_VM_PRIORITY_DIFF :: integer(),
	ivroption :: string() | 'undefined',
	caseid :: string() | 'undefined',
	moh = "moh" :: string() | 'none',
	record_path :: 'undefined' | string(),
	dial_vars :: list(),
	hold :: 'undefined' | 'hold',
	spawn_oncall_mon :: 'undefined' | {pid(), reference()},
	conference_id :: 'undefined' | string(),
	'3rd_party_id' :: 'undefined' | string(),
	'3rd_party_mon' :: 'undefined' | {pid(), reference()},
	in_conference = false :: boolean(),
	outbound_pid :: pid() | 'undefined',
	moh_broadcast = false :: boolean(),
	moratoriumtimer :: reference()
}).

-type(state() :: #state{}).
-define(GEN_MEDIA, true).
-include_lib("reach_core/include/gen_spec.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc starts the freeswitch media gen_media.  `Cnode' is the C node the
%% communicates directly with freeswitch.  Dialstring is the data used to
%% connect to an outside line.  UUID is the id of the channel on
%% freeswith, and therefore the id of the call in OpenACD.
-spec(start/3 :: (Cnode :: atom(), DialOpts :: {string(), list()}, UUID :: string()) -> {'ok', pid()}).
start(Cnode, DialOpts, UUID) ->
	gen_media:start(?MODULE, [Cnode, DialOpts, UUID]).

start(Cnode, DialOpts, UUID, Queue) ->
	gen_media:start(?MODULE, [Cnode, DialOpts, UUID, Queue]).

%% @doc Starts the freeswitch gen_media linked to the calling process.
%% @see start/3
-spec(start_link/3 :: (Cnode :: atom(), DialOpts :: {string(), list()}, UUID :: string()) -> {'ok', pid()}).
start_link(Cnode, DialOpts, UUID) ->
	gen_media:start_link(?MODULE, [Cnode, DialOpts, UUID]).

%% @doc returns the record of the call freeswitch media `MPid' is in
%% charge of.
-spec(get_call/1 :: (MPid :: pid()) -> #call{}).
get_call(MPid) ->
	gen_media:get_call(MPid).

%% @doc A debugging function that returns the state record.
-spec(dump_state/1 :: (Mpid :: pid()) -> #state{}).
dump_state(Mpid) when is_pid(Mpid) ->
	gen_media:call(Mpid, dump_state).

%% @hidden
'3rd_party_pickup'(Mpid) ->
	Self = self(),
	gen_media:cast(Mpid, {'3rd_party_pickup', Self}).

%%====================================================================
%% gen_media callbacks
%%====================================================================

%% @private
init([Cnode, DialOpts, UUID]) ->
	process_flag(trap_exit, true),
	Manager = whereis(freeswitch_media_manager),
	{DialString, CustomHeadersNames} = DialOpts,
	lager:info("Custom headers to set: ~p", [CustomHeadersNames]),
	{DNIS, Client, Priority, CidName, CidNum, SIPFrom, ExportVars, Headers} = get_info(Cnode, UUID, CustomHeadersNames),

	CallPs = [
		{id, UUID},
		{client, Client}, %% id :: string()
		{priority, Priority},
		{caller_id, {CidName, CidNum}},
		{dnis, DNIS},
		{media_path, inband}
	],

	State = #state{
		statename = inivr,
		cnode=Cnode,
		manager_pid = Manager,
		dialstring = DialString,
		dial_vars = ["sip_h_X-FromData='"++SIPFrom++"'"] ++ ExportVars ++ Headers,
		uuid = UUID
	},

	gproc:reg({p, g, {freeswitch_node, UUID}}, Cnode),
	{ok, State, CallPs};

init([Cnode, DialOpts, UUID, Queue]) ->
	process_flag(trap_exit, true),
	{DialString, CustomHeadersNames} = DialOpts,
	Manager = whereis(freeswitch_media_manager),
	{DNIS, Client, Priority, CidName, CidNum, SIPFrom, ExportVars, Headers} = get_info(Cnode, UUID, CustomHeadersNames),

	CallPs = [
		{id, UUID},
		{client, Client}, %% id :: string()
		{priority, Priority},
		{caller_id, {CidName, CidNum}},
		{dnis, DNIS},
		{media_path, inband},
		{queue, Queue}
	],

	State = #state{
		statename = inqueue,
		cnode=Cnode,
		manager_pid = Manager,
		dialstring = DialString,
		dial_vars = ["sip_h_X-FromData='"++SIPFrom++"'"] ++ ExportVars ++ Headers,
		uuid = UUID
	},

	gproc:reg({p, g, {freeswitch_node, UUID}}, Cnode),
	{ok, State, CallPs}.

%% @private
-spec(urlpop_getvars/1 :: (State :: #state{}) -> [{binary(), binary()}]).
urlpop_getvars(#state{ivroption = Ivropt} = _State) ->
	[{"itxt", Ivropt}].

get_monitor_leg(#state{ringuuid = RingUuid} = _State) ->
	RingUuid.

%% @private
-spec(handle_announce/5 :: (Announcement :: string(), StateName :: atom(), Call :: #call{}, Internal :: any(), State :: #state{}) -> {'ok', #state{}}).
handle_announce(Announcement, _StateName, Call, _Internal, State) ->
	case State#state.moh_broadcast of
		true ->
			% MOH being broadcasted during queue transfer
			% break before playing announcement, then replay on playback stop
			freeswitch:api(State#state.cnode, uuid_break, Call#call.id),
			freeswitch:api(State#state.cnode, uuid_broadcast, Call#call.id ++ " playback::{play_moh_if_inqueue=true}" ++ Announcement),
			case State#state.moh of
				none ->
					ok;
				_MohMusak ->
					freeswitch:bgapi(State#state.cnode, uuid_broadcast, Call#call.id ++ " playback::local_stream://" ++ State#state.moh)
			end;
		_ ->
			freeswitch:sendmsg(State#state.cnode, Call#call.id,
				[{"call-command", "execute"},
					{"execute-app-name", "playback"},
					{"execute-app-arg", Announcement}])
	end,
	{ok, State}.

%%--------------------------------------------------------------------
%% perpare_endpoint
%%--------------------------------------------------------------------

-type(agent_registration_type() :: 'sip' | 'sip_registration' | 'h323' |
	'iax2' | 'pstn').
-type(agent_registration_data_opt() :: {'date', string()}).
-type(agent_registration_type_opt() :: {'type', agent_registration_type()}).
-type(persistant_ring_opt() :: 'persistant').
-type(endpoint_opt() :: persistant_ring_opt() |
	agent_registration_type_opt() | agent_registration_data_opt()).
-type(endpoint_opts() :: [endpoint_opt()]).
-type(freeswitch_ring_endpoint() :: pid() | {'freeswitch_ring','start',[_]}).
%% @doc Handle an agent being setup with a freeswitch_media endpoint.
%% Options uses defaults of [{type,sip_registration},{data,agent_login()}].
-spec(prepare_endpoint/2 :: (Agent :: #agent{}, Options :: endpoint_opts())
	-> {'ok', freeswitch_ring_endpoint()}).
prepare_endpoint(Agent, Options) ->
	{Node, Dialstring, Dest} = freeswitch_media_manager:get_ring_data(Agent, Options),
	RingOpts = [{freeswitch_node, Node}, {dialstring, Dialstring}, {destination, Dest}],
	case proplists:get_value(persistant, Options) of
		true ->
			cpx_endpoint:start(freeswitch_ring, [{module, freeswitch_ring_persistent}|RingOpts]);
		_ ->
			{ok, {freeswitch_ring, [{module, freeswitch_ring_transient}|RingOpts]}}
	end.

%%--------------------------------------------------------------------
%% handle_answer
%%--------------------------------------------------------------------

%% @hidden
handle_answer(Apid, StateName, Callrec, GenMediaState, State) when
		StateName =:= inqueue_ringing; StateName =:= oncall_ringing ->
	{Agent, RingUUID, RingPid} = case GenMediaState of
		#inqueue_ringing_state{outband_ring_pid = undefined} ->
			{undefined, "", undefined};
		#inqueue_ringing_state{outband_ring_pid = P, ring_pid = {Agt, _Apid}} ->
			{Agt, freeswitch_ring:get_uuid(P), P};
		#oncall_ringing_state{outband_ring_pid = undefined} ->
			{undefined, "", undefined};
		#oncall_ringing_state{outband_ring_pid = P, ring_pid = {Agt, _Apid}} ->
			{Agt, freeswitch_ring:get_uuid(P), P}
	end,
	CallId = Callrec#call.id,
	Client = Callrec#call.client,
	case freeswitch:api(State#state.cnode, uuid_bridge, CallId ++ " " ++ RingUUID) of
		{ok, _} ->
			% RecPath = case cpx_supervisor:get_archive_path(Callrec) of
			% 	none ->
			% 		lager:debug("archiving is not configured for ~p", [Callrec#call.id]),
			% 		undefined;
			% 	{error, _Reason, Path} ->
			% 		lager:warning("Unable to create requested call archiving directory for recording ~p for ~p", [Path, Callrec#call.id]),
			% 		undefined;
			% 	Path ->
			% 		% get_archive_path ensures the directory is writeable by us and
			% 		% exists, so this should be safe to do (the call will be hungup
			% 		% if creating the recording file fails)
			% 		lager:debug("archiving ~p to ~s.wav", [Callrec#call.id, Path]),
			% 		freeswitch:api(State#state.cnode, uuid_setvar, Callrec#call.id ++ " RECORD_APPEND true"),
			% 		freeswitch:api(State#state.cnode, uuid_record, Callrec#call.id ++ " start "++Path++".wav"),
			% 		Path++".wav"
			% end,
			RecPath = case get_recording_path(Callrec) of
				undefined ->
					lager:info("archiving is not configured for ~p, client ~p", [CallId, Client#client.id]),
					undefined;
				Path ->
					% get_archive_path ensures the directory is writeable by us and
					% exists, so this should be safe to do (the call will be hungup
					% if creating the recording file fails)
					lager:info("archiving ~p to ~p", [CallId, Path]),

                    ApiReq1 = CallId ++ " RECORD_APPEND=true;recording_follow_transfer=true",
                    ApiReq2 = CallId ++ " start " ++ Path,

                    lager:info("archiving uuid_setvar_multi on ~p ",[ApiReq1]),
					case freeswitch:api(State#state.cnode, uuid_setvar_multi, ApiReq1) of 
                        {ok, Answ1} -> lager:info("Answer on record enable is ~p",[Answ1]);
                        {error,Err1} -> lager:warning("The api request failed with ~p",[Err1]);
                        timeout ->  lager:warning("The api request timed out");
                        Other1 -> lager:warning("Unexpected api answer ~p",[Other1])
                    end,
					case freeswitch:api(State#state.cnode, uuid_record, ApiReq2) of
                        {ok, Answ2} -> lager:info("Answer on record enable is ~p",[Answ2]);
                        {error,Err2} -> lager:warning("The api request failed with ~p",[Err2]);
                        timeout ->  lager:warning("The api request timed out");
                        Other2 -> lager:warning("Unexpected api answer ~p",[Other2])
                    end,
					Path
			end,
			agent_channel:media_push(Apid, Callrec, {mediaload, Callrec, [{<<"height">>, <<"300px">>}, {<<"title">>, <<"Server Boosts">>}]}),
			freeswitch:api(State#state.cnode, uuid_setvar, CallId ++ " reach_state oncall"),
			NewState = case State#state.initial_agent of
				undefined ->
					State#state{agent_pid = Apid, initial_agent = Agent, agent = Agent,
						record_path = RecPath, queued = false, statename = oncall,
						ringchannel = RingPid, ringuuid = RingUUID, moh_broadcast = false};
				_ ->
					State#state{agent_pid = Apid, agent = Agent,
						record_path = RecPath, queued = false, statename = oncall,
						ringchannel = RingPid, ringuuid = RingUUID, moh_broadcast = false}
				end,
			{ok, NewState};
		{error, Error} ->
			lager:warning("Bridge of ~p and ~p failed with error:  ~p. ", [CallId, RingUUID, Error]),
			{error, Error, State}
	end.


% TODO added for testing only (implemented with focus on real Calls - no other media)
%% @hidden
-spec(handle_end_call/4 :: (GMState :: atom(), Callrec :: #call{},
GMStateData :: any(), State :: #state{}) -> {'ok', #state{}}).
handle_end_call(_Gmstate, Callrec, _Gmstatedata, State) ->
	freeswitch:sendmsg(State#state.cnode, Callrec#call.id,
		[{"call-command", "hangup"},
			{"hangup-cause", "SUBSCRIBER_ABSENT"}]),
	{deferred, State}.

%% @hidden
handle_ring(Apid, RingData, Callrec, State) when is_pid(Apid) ->
	lager:info("ring to agent ~p for call ~s", [Apid, Callrec#call.id]),
	% TODO - we could avoid this if we had the agent's login,
	AgentRec = agent:dump_state(Apid),
	handle_ring({Apid, AgentRec}, RingData, Callrec, State);
handle_ring({_Apid, #agent{ring_channel = {undefined, persistant, _}} = Agent}, _RingData, _Callrec, State) ->
	lager:warning("Agent (~p) does not have it's persistant channel up yet", [Agent#agent.login]),
	{invalid, State};
handle_ring({Apid, #agent{ring_channel = {EndpointPid, persistant, _EndPointType}}} = _Agent, _RingData, Callrec, State) ->
	%% a persisitant ring does the hard work for us
	%% go right to the okay.
	lager:info("Ring channel made things happy, I assume", []),
	{ok, [{"itxt", State#state.ivroption}], Callrec#call{ring_path = inband, media_path = inband}, State#state{ringchannel = EndpointPid, agent_pid = Apid}};
handle_ring({Apid, #agent{ring_channel = {RPid, transient, _}} = _AgentRec}, _RingData, _Callrec, State) ->
	NewStatename = case State#state.statename of
		inqueue -> inqueue_ringing;
		oncall -> oncall_ringing
	end,
	{ok, [{"itxt", State#state.ivroption}], State#state{statename = NewStatename, agent_pid = Apid, ringchannel = RPid}}.

% TODO This needs to be updated when conferencing is fixed.
%% @hidden
handle_ring_stop(_StateName, Callrec, _GenMedia, #state{xferchannel = RingChannel} = State) when is_pid(RingChannel) ->
	lager:debug("hanging up transfer channel for ~p", [Callrec#call.id]),
	freeswitch_ring:hangup(RingChannel),
	{ok, State#state{xferchannel = undefined, xferuuid = undefined}};

handle_ring_stop(StateName, Callrec, _GenMedia, State) ->
	lager:debug("hanging up ring channel for ~p", [Callrec#call.id]),
	case State#state.ringchannel of
		undefined ->
			ok;
		RingChannel ->
			% TODO - make sure the call didn't get bridged in the interim?
			% the ring channel might have bridged and the message is sitting
			% in our mailbox
			freeswitch_ring:hangup(RingChannel)
	end,
	NewStatename = case StateName of
		inqueue_ringing -> inqueue;
		oncall_ringing -> oncall
	end,
	{ok, State#state{statename = NewStatename, ringchannel=undefined}}.

%% @hidden
-spec(handle_voicemail/4 :: (StateName :: atom(), Call :: #call{}, Internal :: any(), State :: #state{}) -> {'ok', #state{}}).
handle_voicemail(inqueue_ringing, Callrec, Internal, State) ->
	{ok, Midstate} = handle_ring_stop(inqueue_ringing, Callrec, undefined, State),
	handle_voicemail(undefined, Callrec, Internal, Midstate);
handle_voicemail(_, Call, _, State) ->
	UUID = Call#call.id,
	freeswitch:bgapi(State#state.cnode, uuid_transfer, UUID ++ " 'playback:IVR/prrec.wav,gentones:%(500\\,0\\,500),sleep:600,record:/tmp/${uuid}.wav' inline"),
	{ok, State#state{statename = inivr, voicemail = "/tmp/"++UUID++".wav"}}.

-spec(handle_transfer_outband/5 :: (Addr :: any(), StateName :: atom(), Call :: #call{}, Internal :: any(), State :: #state{}) -> {'ok', #state{}}).
handle_transfer_outband(Addr, inqueue_ringing, Callrec, Internal, State) ->
	{ok, Midstate} = handle_ring_stop(inqueue_ringing, Callrec, undefined, State),
	handle_transfer_outband(Addr, undefined, Callrec, Internal, Midstate);
handle_transfer_outband(Addr, Statename, Call, GenMediaState, St) ->
	{_, St1} = handle_cast({blind_transfer, Addr}, Statename, Call, GenMediaState, St),
	{ok, St1}.

%% @hidden
-spec(handle_spy/5 :: (Spy :: {pid(), #agent{}}, StateName :: atom(), Call :: #call{}, Internal :: #state{}, State :: any()) -> {'error', 'bad_agent', #state{}} | {'ok', #state{}}).
handle_spy({Agent, AgentRec}, oncall, Call, #state{cnode = Fnode, ringchannel = Chan} = Internal, _State) when is_pid(Chan) ->
	agent:blab(Agent, "While spying, you have the following options:\n"++
		"* To whisper to the agent; press 1\n"++
		"* To whisper to the caller; press 2\n"++
		"* To talk to both parties; press 3\n"++
		"* To resume spying; press 0"),
	Dialstring = freeswitch_media_manager:get_agent_dial_string(AgentRec, []),
	freeswitch:bgapi(Fnode, originate, Dialstring ++ " &eavesdrop(" ++ Call#call.id ++ ")"),
	{ok, Internal};
handle_spy(_Spy, _StateName, _Call, _Internal, State) ->
	{invalid, State}.

%% @hidden
handle_agent_transfer(AgentPid, Timeout, Call, State) ->
	AgentRec = agent:dump_state(AgentPid), % TODO - avoid this
	lager:info("transfer_agent to ~p for call ~p", [AgentRec#agent.login, Call#call.id]),
	% fun that returns another fun when passed the UUID of the new channel
	% (what fun!)
	F = fun(_UUID) ->
		fun(ok, _Reply) ->
			% agent picked up?
				lager:info("Agent transfer picked up? ~p", [Call#call.id]);
		(error, Reply) ->
			lager:warning("originate failed for ~p with  ~p", [Call#call.id, Reply])
		end
	end,
	case freeswitch_ring:start_link(State#state.cnode, AgentRec, AgentPid, Call, Timeout, F, [single_leg, no_oncall_on_bridge, {dial_vars, State#state.dial_vars}]) of
		{ok, Pid} ->
			{ok, [{"ivropt", State#state.ivroption}, {"caseid", State#state.caseid}], State#state{statename = oncall_ringing, xferchannel = Pid, xferuuid = freeswitch_ring:get_uuid(Pid)}};
		{error, Error} ->
			lager:error("error:  ~p", [Error]),
			{error, Error, State}
	end.

%% @hidden
handle_wrapup(_From, _StateName, #call{media_path = inband} = _Call, _GenMediaState,
	#state{ringuuid=RingUUID} = State) ->
	lager:warning("Handling wrapup request", []),
	% TODO This could prolly stand to be a bit more elegant.
	freeswitch:api(State#state.cnode, uuid_kill, RingUUID),
	{hangup, State};
handle_wrapup(_From, _StateName, _Call, _GenMediaState, State) ->
	% This intentionally left blank; media is out of band, so there's
	% no direct hangup by the agent
	lager:debug("Not doing wrapup request", []),
	{ok, State}.

%% @hidden
handle_queue_transfer({QName, _Qpid}, _StateName, Call, _GenMediaState, #state{cnode = Fnode} = State) ->
	case State#state.record_path of
		undefined ->
			ok;
		Path ->
			lager:debug("stopping recording due to queue transfer for ~p", [Call#call.id]),
			freeswitch:api(Fnode, uuid_record, Call#call.id ++ " stop " ++ Path)
	end,
	% play musique d'attente
	% TODO this can generate an annoying warning in FS, but I don't care
	% right now
	freeswitch:api(Fnode, uuid_park, Call#call.id),
	case State#state.moh of
		none ->
			ok;
		_MohMusak ->
			freeswitch:api(Fnode, uuid_setvar, Call#call.id ++ " ignore_early_media ring_ready"),
			freeswitch:api(Fnode, uuid_broadcast, Call#call.id ++ " playback::local_stream://" ++ State#state.moh)
	end,
	{ok, State#state{statename = inqueue, queue = QName, queued = true, agent_pid = undefined, moh_broadcast = true}}.

handle_conference_to_queue({LegId, Request}, Call, Queue, Opts, _GenMediaState,
		#state{cnode=Cnode, uuid=CallId, ringuuid=RingUUID,
			agent=Agent, in_conference=InConf} = State)
		when is_list(RingUUID) ->

	#call{priority=Priority, client=Client, dnis=Dnis, url_vars=UrlVars} = Call,
	ClientId = Client#client.id,

	Info = [{dnis, Dnis}, {url_vars, UrlVars}],

	case InConf of
		false ->
			lager:info("Converting ~p to a conference", [CallId]),
			freeswitch:api(Cnode, uuid_setvar, CallId ++ " hangup_after_bridge false"),
			freeswitch:api(Cnode, uuid_setvar, RingUUID ++ " hangup_after_bridge false"),
			case State#state.hold of
				% Converting bgapi to api to avoid transfer to conf and play moh race condition
				hold ->
					freeswitch:api(Cnode, uuid_transfer, CallId ++ " -both conference:" ++
						CallId ++ "@reach++flags{endconf} inline"),
					conference_manager:start(CallId, self(), Agent, [{fs_node, Cnode}, {hold, true}, {fs_uuid, RingUUID}]);
				_ ->
					freeswitch:api(Cnode, uuid_transfer, CallId ++ " -both conference:" ++
						CallId ++ "@reach++flags{endconf} inline"),
					conference_manager:start(CallId, self(), Agent, [{fs_node, Cnode}, {fs_uuid, RingUUID}])
			end;
		_ ->
			ok
	end,

	case proplists:get_value(transfer, Opts) of
		true ->
			conference_manager:transfer_leg(CallId, LegId);
		_ ->
			ok
	end,

	freeswitch_media_manager:new_conference({LegId, Request}, Call, RingUUID, Queue, Priority,
		ClientId, Info),
	{ok, State#state{in_conference = true}}.

handle_conference_to_outband(Call, Dest, ARec, LegId, Opts, _GenMediaState,
		#state{cnode=Cnode, ringuuid=RingUUID, in_conference=InConf} = State)
		when is_list(RingUUID) ->

	#call{id=CallId, callerid={CallerId, CallerNum}} = Call,
	#agent{login=Agent, source=AgentPid, connection=Conn} = ARec,

	Props = [
		{agent, Agent}, {agent_pid, AgentPid}, {conn, Conn},
		{destination, Dest}, {uuid, CallId}, {type, conference},
		{call, Call}, {caller_id, CallerId}, {caller_number, CallerNum},
		{conf_id, LegId}
	],

	case InConf of
		false ->
			freeswitch:api(Cnode, uuid_setvar, CallId ++ " hangup_after_bridge false"),
			freeswitch:api(Cnode, uuid_setvar, RingUUID ++ " hangup_after_bridge false"),
			case State#state.hold of
				hold ->
					% Converting bgapi to api to avoid transfer to conf and play moh race condition
					freeswitch:api(Cnode, uuid_transfer, CallId ++ " -both conference:" ++
						CallId ++ "@reach++flags{endconf} inline"),
					conference_manager:start(CallId, self(), Agent, [{fs_node, Cnode}, {hold, true}, {fs_uuid, RingUUID}]);
				_ ->
					freeswitch:api(Cnode, uuid_transfer, CallId ++ " -both conference:" ++
						CallId ++ "@reach++flags{endconf} inline"),
					conference_manager:start(CallId, self(), Agent, [{fs_node, Cnode}, {fs_uuid, RingUUID}])
			end;
		_ ->
			ok
	end,

	case proplists:get_value(transfer, Opts) of
		true ->
			conference_manager:transfer_leg(CallId, LegId);
		_ ->
			ok
	end,

	Reply = freeswitch_outbound:start_link(Cnode, Props),
	case Reply of
		{ok, OutboundPid} ->
			{{ok, OutboundPid}, State#state{in_conference = true, outbound_pid = OutboundPid}};
		_ ->
			{error, State}
	end.

handle_leave_conference({_OrigMedia,_},_GenMediaState,#state{uuid = UUID} = State) ->
	RingUuid = State#state.ringuuid,
	freeswitch:api(State#state.cnode, uuid_kill, RingUuid),
	conference_manager:update_leg_state(UUID, ?ORIGINAL_LEG_ID, ended),
	{hangup, State}.

handle_drop_external(OutboundPid, _GenmediaState, State) ->
	freeswitch_outbound:stop(OutboundPid),
	{ok, State}.

handle_voicemail_outbound(_Dest, _AgentRec, _Statename, _Call, _GenmediaState, State) ->
	{ok, State}.

handle_transfer_to_node(Node, GenMediaArgs, #state{uuid = UUID} = State) ->
	lager:info("Transferring ~p", [UUID]),
	freeswitch_media_manager:transfer_to_node(Node, UUID, GenMediaArgs),
	{ok, State}.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------

%% @private
handle_call(get_call, _From, _, Call, _, State) ->
	{reply, Call, State};
handle_call(get_agent, _From, _, _Call, _, State) ->
	{reply, State#state.agent_pid, State};
handle_call({set_agent, Agent, Apid}, _From, _, _Call, _, State) ->
	{reply, ok, State#state{agent = Agent, agent_pid = Apid}};
handle_call(dump_state, _From, _, _Call, _, State) ->
	{reply, State, State};

handle_call(Msg, _From, _, Call, _, State) ->
	lager:info("unhandled mesage ~p for ~p", [Msg, Call#call.id]),
	{reply, ok, State}.




%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private

handle_cast(toggle_hold, _Statename, Call, _GenMediaState, #state{statename = Statename} = State)
		when Statename == oncall; Statename == oncall_ringing ->
	lager:debug("toggle hold while oncall", []),
	#state{cnode = Fnode, moh = Muzak, ringuuid = Ringid} = State,
	#call{id = Callid} = Call,
	lager:info("Gonna try to set ~s on hold", [Call#call.id]),
	freeswitch:api(Fnode, uuid_setvar_multi, Callid ++ " hangup_after_bridge=false;park_after_bridge=true"),
	lager:debug("Ringid:  ~p", [Ringid]),
	ok = fs_send_execute(Fnode, Ringid, "set", "hangup_after_bridge=false"),
	ok = fs_send_execute(Fnode, Ringid, "set", "park_after_bridge=true"),
	Res = freeswitch:api(Fnode, uuid_transfer, Ringid ++ " park inline"),
	lager:error("Res of the api:  ~p", [Res]),
	case Muzak of
		none -> ok;
		_ ->
			lager:error("5 ~p", [fs_send_execute(Fnode, Callid, "playback", "local_stream://" ++ Muzak)])
	end,
	{noreply, State#state{statename = oncall_hold}};

%% oncall hold -> next_state
handle_cast(toggle_hold, _Statename, Call, _GenMediaState, #state{statename = oncall_hold} = State) ->
	lager:info("Gonna try to pick up the holder dude for ~s", [Call#call.id]),
	#state{cnode = Fnode, ringuuid = Ringid} = State,
	#call{id = Callid} = Call,
	freeswitch:api(Fnode, uuid_bridge, Callid ++ " " ++ Ringid),
	freeswitch:api(Fnode, uuid_setvar_multi, Callid ++ " hangup_after_bridge=true;park_after_bridge=false"),
	{noreply, State#state{statename = oncall}};

handle_cast({contact_3rd_party, _Args} = Cast, Statename, Call, GenMediaState, #state{statename = oncall_hold, cnode = Fnode} = State) ->
	% first step is to move to hold_conference state, which means
	% creating the conference.
	{ok, ConfId} = freeswitch:api(Fnode, create_uuid),
	case freeswitch:api(Fnode, uuid_transfer, Call#call.id ++ " conference:" ++
		ConfId ++ " inline") of
		{ok, Res} ->
			lager:info("Success result creating conferance and transfering call to it:  ~p", [Res]),
			% okay, solidify the conference state change, and go on.
			Newstate = State#state{conference_id = ConfId, statename = hold_conference},
			handle_cast(Cast, Statename, Call, GenMediaState, Newstate);
		Else ->
			lager:error("Could not create conference:  ~p", [Else]),
			{noreply, State}
	end;

%% hold_conference -> 3rd_party | in_conference
handle_cast({contact_3rd_party, Destination}, _Statename, Call, _GenMediaState, #state{statename = hold_conference, cnode = Fnode} = State) ->
	% start a ring chan to 3rd party
	% play a ringing sound to agent to be nice
	% on any error, we just kill the playback
	% otherwise if the 3rd party picks up, we send a message here to move
	% to the new state.
	#call{client = Client} = Call,
	#client{options = ClientOpts} = Client,
	{CallerNameOpt, CallerNumberOpt} = case proplists:get_value(<<"caller_id">>, ClientOpts) of
		undefined -> {"", ""};
		{BinName, BinNumber} when is_binary(BinName), is_binary(BinNumber) ->
			{binary_to_list(BinName), binary_to_list(BinNumber)};
		CidOut -> CidOut
	end,
	%Destination = binary_to_list(proplists:get_value("args", Args)),
	BaseDs = freeswitch_media_manager:get_default_dial_string(),
	RingOps = [CallerNameOpt, CallerNumberOpt, "park_after_bridge=true"],
	case originate(Fnode, BaseDs, Destination, RingOps) of
		{ok, UUID} ->
			{noreply, State#state{'3rd_party_id' = UUID, statename = hold_conference_3rdparty}};
		Else ->
			lager:warning("Failed to contact 3rd party ~s due to:  ~p", [Destination, Else]),
			{noreply, State}
	end;

handle_cast(retrieve_conference, _Statename, Call, _GenMediaState, #state{
		statename = Statename} = State) when Statename =:= 'hold_conference';
		Statename =:= 'hold_conference_3rdparty' ->
	#state{cnode = Fnode, ringuuid = Ringid, conference_id = Confid} = State,
	freeswitch:api(Fnode, uuid_transfer, Ringid ++ " 'conference:" ++ Confid ++ "' inline"),
	NewState = case Statename of
		'hold_conference' -> 'in_conference';
		'hold_conference_3rdparty' -> 'in_conference_3rdparty'
	end,
	cdr:media_custom(Call, NewState, ?cdr_states, []),
	{{mediapush, NewState}, State#state{statename = NewState}};

% 3rd_party -> hold_conference_3rdparty | in_conference | hold_conference
handle_cast(toggle_hold, _Statename, _Call, _GenMediaState, #state{statename = '3rd_party'} = State) ->
	lager:info("Place 3rd party on hold", []),
	#state{cnode = Fnode, moh = _Muzak, ringuuid = Ringid, '3rd_party_id' = ThirdPId} = State,
	ok = fs_send_execute(Fnode, Ringid, "set", "hangup_after_bridge=false"),
	ok = fs_send_execute(Fnode, Ringid, "set", "park_after_bridge=true"),
	freeswitch:api(Fnode, uuid_transfer, Ringid ++ " park inline"),
	% RETRIEVE_CONFERENCE breaks with this code; just specifying "park"
	% works, though.
	Helddp = "park",
	freeswitch:api(Fnode, uuid_transfer, ThirdPId ++ " " ++ Helddp ++ " inline"),
	{noreply, State#state{statename = hold_conference_3rdparty}};

handle_cast(retrieve_conference, Statename, Call, GenMediaState, #state{statename = '3rd_party'} = State) ->
	lager:info("Place 3rd party on hold, and go to the conference", []),
	{noreply, MidState} = handle_cast(toggle_hold, Statename, Call, GenMediaState, State),
	{noreply, MidState#state{statename = in_conference_3rdparty}};

handle_cast(retrieve_3rd_party, _Statename, _Call, _GenMediaState, #state{statename = 'in_conference_3rdparty'} = State) ->
	lager:info("Taking agent out of conference, briding to 3rdparyt",[]),
	#state{cnode = Fnode, ringuuid = Ringid, '3rd_party_id' = Thirdy} = State,
	freeswitch:api(Fnode, uuid_bridge, Thirdy ++ " " ++ Ringid),
	{noreply, State#state{statename = '3rd_party'}};

handle_cast(hangup_3rd_party, Statename, Call, GenMediaState, #state{statename = '3rd_party'} = State) ->
	lager:info("killing 3rd party channel while talking w/ them", []),
	{noreply, MidState} = handle_cast(toggle_hold, Statename, Call, GenMediaState, State),
	handle_cast(hangup_3rd_party, Statename, Call, GenMediaState, MidState);

handle_cast({merge_3rd_party, _IncludeSelf}, _Statename, _Call, _GenMediaState, #state{'3rd_party_id' = undefined} = State) ->
	{noreply, State};

handle_cast({merge_3rd_party, IncludeAgent}, _Statename, _Call, _GenMediaState, State) ->
	#state{cnode = Fnode, ringuuid = Ringid, '3rd_party_id' = Thirdid, conference_id = Confid} = State,
	NextState = case IncludeAgent of
		true -> 'in_conference';
		_ -> 'hold_conference'
	end,
	freeswitch:api(Fnode, uuid_transfer, Ringid ++ " park inline"),
	freeswitch:bgapi(Fnode, uuid_transfer, Thirdid ++ " conference:" ++ Confid ++ " inline"),
	{noreply, State#state{statename = NextState}};

% retrieve conference also works here.
handle_cast(retrieve_3rd_party, _Statename, _Call, _GenMediaState, #state{statename = hold_conference_3rdparty} = State) ->
	#state{cnode= Fnode, ringuuid = Ringid, '3rd_party_id' = Thirdpid} = State,
	lager:info("Picking up help 3rd party:  ~s", [Thirdpid]),
	freeswitch:bgapi(Fnode, uuid_bridge, Thirdpid ++ " " ++ Ringid),
	{noreply, State#state{statename = '3rd_party'}};

handle_cast(hangup_3rd_party, _Statename, _Call, _GenMediaState, #state{statename = 'hold_conference_3rdparty'} = State) ->
	lager:info("hangling up on 3rd party", []),
	#state{cnode = Fnode, '3rd_party_id' = Thirdid} = State,
	freeswitch:bgapi(Fnode, uuid_kill, Thirdid),
	{noreply, State#state{statename = 'hold_conference'}};

% in_conference -> '3rdparty' | 'conference_hold'
handle_cast(toggle_hold, _Statename, _Call, _GenMediaState, #state{statename = 'in_conference'} = State) ->
	lager:info("Place conference on hold", []),
	#state{cnode = Fnode, ringuuid = Ringid} = State,
	ok = fs_send_execute(Fnode, Ringid, "set", "hangup_after_bridge=false"),
	ok = fs_send_execute(Fnode, Ringid, "set", "park_after_bridge=true"),
	freeswitch:api(Fnode, uuid_transfer, Ringid ++ " park inline"),
	{{mediapush, hold_conference}, State#state{statename = hold_conference}};

handle_cast({contact_3rd_party, _Targ} = Cast, Statename, Call, GenMediaState, #state{statename = 'in_conference'} = State) ->
	lager:info("contact 3rd party, means place conference on hold first", []),
	{noreply, MidState} = handle_cast(toggle_hold, Statename, Call, GenMediaState, State),
	handle_cast(Cast, Statename, Call, GenMediaState, MidState);

% any state.
handle_cast({audio_level, Target, Level}, _Statename, Call, _GenMediaState, #state{statename = Statename} =
		State) when Statename == oncall; Statename == oncall_ringing ->
	lager:info("uuid_audio for ~s with direction ~s set to ~p", [Call#call.id, Target, Level]),
	ApiStr = Call#call.id ++ " start " ++ binary_to_list(Target) ++ " level "
		++ integer_to_list(Level),
	freeswitch:bgapi(State#state.cnode, uuid_audio, ApiStr),
	{noreply, State};

handle_cast({blind_transfer, Destination0}, _Statename, Call, _GenMediaState, #state{statename = Statename} = State)
		when Statename == inqueue; Statename == oncall ->
	Destination = freeswitch_media_manager:fix_sip_addr(Destination0),
	#state{cnode = Fnode} = State,
	#call{client = Client, id = UUID} = Call,
	#client{options= ClientOpts} = Client,
	{CallerNameOpt,CallerNumberOpt} = case proplists:get_value(<<"caller_id">>, ClientOpts) of
		undefined -> {"",""};
		{BinName,BinNumber} when is_binary(BinName),is_binary(BinNumber) ->
			{binary_to_list(BinName),binary_to_list(BinNumber)};
		CidOut -> CidOut
	end,
	BaseDS = freeswitch_media_manager:get_default_dial_string(),
	RingOpts = [CallerNameOpt, CallerNumberOpt, "hangup_after_bridge=true"],
	Dialstring = freeswitch_media_manager:do_dial_string_without_sip_opts(BaseDS, Destination, RingOpts),
	freeswitch:bgapi(Fnode, uuid_setvar, UUID ++ " transfer_ringback local_stream://" ++ State#state.moh),
	freeswitch:bgapi(Fnode, uuid_transfer, UUID ++ " 'm:^:bridge:" ++ Dialstring ++ "' inline"),

	{noreply, State#state{statename = 'blind_transfered'}};

handle_cast({play_dtmf, []}, _Statename, _Call, _GenMediaState, State) ->
	lager:debug("Dtmf not played due to no digits", []),
	{noreply, State};

handle_cast({play_dtmf, Digits}, Statename, Call, GenMediaState, State) when is_binary(Digits) ->
	handle_cast({play_dtmf, binary_to_list(Digits)}, Statename, Call, GenMediaState, State);

handle_cast({play_dtmf, Digits}, _Statename, Call, _GenMediaState, #state{statename = oncall} = State) ->
	#state{cnode = Fnode} = State,
	#call{id = UUID} = Call,
	freeswitch:bgapi(Fnode, uuid_send_dtmf, UUID ++ " " ++ Digits),
	{noreply, State};

%% web api's
handle_cast({agent_web_connection, <<"toggle_hold">>, _}, Statename, Call, GenMediaState, State) ->
	handle_cast(toggle_hold, Statename, Call, GenMediaState, State);

handle_cast({agent_web_connection, <<"retrieve_3rd_party">>, _}, Statename, Call, GenMediaState, State) ->
	handle_cast(retrieve_3rd_party, Statename, Call, GenMediaState, State);

handle_cast({agent_web_connection, <<"contact_3rd_party">>, Args}, Statename, Call, GenMediaState, State) ->
	Destination = binary_to_list(proplists:get_value("args", Args)),
	handle_cast({contact_3rd_party, Destination}, Statename, Call, GenMediaState, State);

handle_cast({agent_web_connection, <<"retrieve_conference">>, _Args}, Statename, Call, GenMediaState, State) ->
	handle_cast(retrieve_conference, Statename, Call, GenMediaState, State);

handle_cast({agent_web_connection, <<"merge_3rd_party">>, Args}, Statename, Call, GenMediaState, State) ->
	IncludeAgent = case proplists:get_value("args", Args) of
		<<"true">> ->
			true;
		_ ->
			false
	end,
	handle_cast({merge_3rd_party, IncludeAgent}, Statename, Call, GenMediaState, State);

handle_cast({agent_web_connection, <<"hangup_3rd_party">>, _}, Statename, Call, GenMediaState, State) ->
	handle_cast(hangup_3rd_party, Statename, Call, GenMediaState, State);

handle_cast({agent_web_connection, <<"audio_level">>, Arguments}, Statename, Call, GenMediaState, State) ->
	[Target, Level] = case proplists:get_value("args", Arguments, [<<"read">>, 0]) of
		[<<"write">>, N] -> [write, N];
		[_,N] -> [read,N];
		_ -> [read,0]
	end,
	handle_cast({audio_level, Target, Level}, Statename, Call, GenMediaState, State);

handle_cast({agent_web_connection, <<"blind_transfer">>, Args}, Statename, Call, GenMediaState, State) ->
	Dest = proplists:get_value("args", Args),
	handle_cast({blind_transfer, Dest}, Statename, Call, GenMediaState, State);

handle_cast({agent_web_connection, <<"play_dtmf">>, Args}, Statename, Call, GMState, State) ->
	Digits = case proplists:get_value("args", Args, []) of
		X when is_integer(X) -> integer_to_list(X);
		X -> X
	end,
	handle_cast({play_dtmf, Digits}, Statename, Call, GMState, State);

%% tcp api's
handle_cast(Request, Statename, Call, GenMediaState, State) when is_record(Request, mediacommandrequest) ->
	FixedRequest = cpx_freeswitch_pb:decode_extensions(Request),
	Hint = case cpx_freeswitch_pb:get_extension(FixedRequest, freeswitch_request_hint) of
		{ok, O} -> O;
		_ -> undefined
	end,
	case Hint of
		'SET_AUDIO_LEVEL' ->
			case cpx_freeswitch_pb:get_extension(audio_level_request,FixedRequest) of
				#audiolevelrequest{channel = X, value = Y} when X == undefined; Y == undefined ->
					{noreply, State};
				#audiolevelrequest{channel = Chan, value = Val} ->
					Target = case Chan of
						'SPEAKER' -> write;
						'MIC' -> read
					end,
					handle_cast({audio_level, Target, Val}, Statename, Call, GenMediaState, State);
				_ ->
					{noreply, State}
			end;
		'TOGGLE_HOLD' ->
			handle_cast(toggle_hold, Statename, Call, GenMediaState, State);
		'CONTACT_3RD_PARTY' ->
			case cpx_freeswitch_pb:get_extension(FixedRequest, contact_3rd_party) of
				undefined -> handle_cast(contact_3rd_party, Statename, Call, GenMediaState, State);
				{ok, #contact3rdpartyrequest{target = Target}} ->
					handle_cast({contact_3rd_party, Target}, Statename, Call, GenMediaState, State)
			end;
		'RETRIEVE_CONFERENCE' -> handle_cast(retrieve_conference, Statename, Call, GenMediaState, State);
		'MERGE_3RD_PARTY' ->
			case cpx_freeswitch_pb:get_extension(FixedRequest, merge_3rd_party) of
				undefined ->
					handle_cast({merge_3rd_party, false}, Statename, Call, GenMediaState, State);
				{ok, #merge3rdpartyrequest{include_self = AndSelf}} ->
					handle_cast({merge_3rd_party, AndSelf}, Statename, Call, GenMediaState, State)
			end;
		'RETRIEVE_3RD_PARTY' ->
			handle_cast(retrieve_3rd_party, Statename, Call, GenMediaState, State);
		'HANGUP_3RD_PARTY' ->
			handle_cast(hangup_3rd_party, Statename, Call, GenMediaState, State);
		'BLIND_TRANSFER' ->
			case cpx_freeswitch_pb:get_extension(FixedRequest,blind_transfer) of
				{ok, #blindtransferrequest{target = Dest}} ->
					handle_cast({blind_transfer, Dest}, Statename, Call, GenMediaState, State);
				BTIgnored ->
					lager:debug("blind transfer ignored:  ~p", [BTIgnored]),
					{noreply, State}
			end;
		'PLAY_DTMF' ->
			case cpx_freeswitch_pb:get_extension(FixedRequest, dtmf_string) of
				{ok, []} ->
					lager:debug("dtmf request ignored due to lack of digits", []),
					{noreply, State};
				{ok, Dtmf} ->
					handle_cast({send_dtmf, Dtmf}, Statename, Call, GenMediaState, State);
				DtmfIgnored ->
					lager:debug("dtmf request ignored:  ~p", [DtmfIgnored]),
					{noreply, State}
			end;
		FullReqIgnored ->
			lager:debug("Request fully ignored:  ~p", [FullReqIgnored]),
			{noreply, State}
	end;

handle_cast({'3rd_party_pickup', ChanPid}, _Statename, _Call, _GenMediaState, #state{'3rd_party_mon' = {ChanPid, _ChanMon}} = State) ->
	#state{cnode = Fnode, '3rd_party_id' = OtherParty, ringuuid = AgentChan} = State,
	freeswitch:bgapi(Fnode, uuid_bridge, OtherParty ++ " " ++ AgentChan),
	{noreply, State#state{statename = '3rd_party'}};

handle_cast({set_caseid, CaseID}, _Statename, Call, _GenMediaState, State) ->
	lager:info("setting caseid for ~p to ~p", [Call#call.id, CaseID]),
	{noreply, State#state{caseid = CaseID}};

handle_cast(Msg, _, _Call, _, State) ->
	lager:debug("unhandled cast while in state ~p:  ~p", [State#state.statename, Msg]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private
handle_info(check_recovery, _StateName, Call, _Internal, State) ->
	case whereis(freeswitch_media_manager) of
		Pid when is_pid(Pid) ->
			link(Pid),
			gen_server:cast(freeswitch_media_manager, {notify, Call#call.id, self()}),
			{noreply, State#state{manager_pid = Pid}};
		_Else ->
			{ok, Tref} = timer:send_after(1000, check_recovery),
			{noreply, State#state{manager_pid = Tref}}
	end;

handle_info(moratorium_timeout, _StateName, Call, _Internal, State) ->
  lager:info("timed out waiting for channel hangup ~p", [Call#call.id]),
  {stop, normal, State};

handle_info({'EXIT', Pid, Reason}, StateName, Call, _Internal,
		#state{ringchannel = Pid} = State) when StateName =:= oncall_ringing;
		StateName =:= inqueue_ringing ->
	lager:warning("Handling ring channel ~w exit ~p for ~p", [Pid, Reason, Call#call.id]),
	{stop_ring, State#state{ringchannel = undefined}};

handle_info({'EXIT', Pid, noconnection}, _StateName, _Call, _Internal,
		State) ->
	lager:warning("Exit of ~p due to noconnection; this normally indicates a fatal freeswitch failure, so going down too.", [Pid]),
	{stop, noconnection, State};

handle_info({'EXIT', Pid, Reason}, _StateName, Call, _Internal, #state{manager_pid = Pid} = State) ->
	lager:warning("Handling manager exit from ~w due to ~p for ~p", [Pid, Reason, Call#call.id]),
	{ok, Tref} = timer:send_after(1000, check_recovery),
	{noreply, State#state{manager_pid = Tref}};

%% TODO make awesome by taking advantage of the state machine-ness.
handle_info({call, {event, [UUID | Rest]}}, _StateName, Call, _Internal, State) when is_list(UUID) ->
	SetSess = freeswitch:session_setevent(State#state.cnode, [
		'CHANNEL_BRIDGE', 'CHANNEL_PARK', 'CHANNEL_HANGUP',
		'CHANNEL_HANGUP_COMPLETE', 'CHANNEL_DESTROY', 'DTMF',
		'CHANNEL_ANSWER', 'PLAYBACK_STOP']),
	lager:debug("reporting new call ~p (eventage:  ~p).", [UUID, SetSess]),
	case State#state.uuid of
		UUID -> freeswitch_media_manager:notify(UUID, self());
		_ -> ok
	end,
	case_event_name([UUID | Rest], Call, State#state{in_control = true});

handle_info({call_event, {event, [UUID | Rest]}}, _StateName, Call, _Internal, State) when is_list(UUID) ->
	case_event_name([ UUID | Rest], Call, State);

handle_info({set_agent, Login, Apid}, _StateName, _Call, _Intenral, State) ->
	lager:info("Setting agent to ~p, ~p", [Login, Apid]),
	{noreply, State#state{agent = Login, agent_pid = Apid}};

handle_info({bgok, Reply}, _StateName, Call, _Internal, State) ->
	lager:debug("bgok:  ~p for ~p", [Reply, Call#call.id]),
	{noreply, State};

handle_info({bgerror, "-ERR NO_ANSWER\n"}, _StateName, Call, _Internal, State) ->
	lager:info("Potential ringout.  Statecook:  ~p for ~p", [State#state.cook, Call#call.id]),
	%% the apid is known by gen_media, let it handle if it is not not.
	{stop_ring, State};

handle_info({bgerror, "-ERR USER_BUSY\n"}, _StateName, Call, _Internal, State) ->
	lager:notice("Agent rejected the call ~p", [Call#call.id]),
	{stop_ring, State};

handle_info({bgerror, Reply}, _StateName, Call, _Internal, State) ->
	lager:warning("unhandled bgerror: ~p for ~p", [Reply, Call#call.id]),
	{noreply, State};

handle_info(channel_destroy, _StateName, Call, _Internal, #state{moratoriumtimer = Timer, in_control = InControl} = State) when not InControl ->
	lager:notice("Hangup in IVR for ~p, cancelling moratorium timer...", [Call#call.id]),
	case Timer of
		undefined ->
			lager:warning("Cannot cancel a undefined timer", []);
		_ ->
			erlang:cancel_timer(Timer)
	end,
	{stop, hangup, State};
% TODO This had a use at some point, but was cuasing ramdom hangups.
% need to find what was sending :(
%handle_info(call_hangup, Call, State) ->
%	lager:notice("Call hangup info, terminating ~p", [Call#call.id]),
%	catch freeswitch_ring:hangup(State#state.ringchannel),
%	{stop, normal, State};

handle_info({'DOWN', Ref, process, Pid, Cause}, _Statename, Call,
		_Internal, #state{statename = oncall,
		spawn_oncall_mon = {Pid, Ref}} = State) ->
	lager:debug("Oncaller pid termination cause:  ~p", [Cause]),
	cdr:media_custom(Call, oncall, ?cdr_states, []),
	{{mediapush, caller_offhold}, State#state{spawn_oncall_mon = undefined}};

handle_info(call_hangup, _StateName, Call, _Internal, State) ->
	lager:notice("Call hangup info, terminating ~p", [Call#call.id]),
	catch freeswitch_ring:hangup(State#state.ringchannel),
	{stop, normal, State};

handle_info(Info, _StateName, Call, _Internal, State) ->
    lager:info("unhandled info ~p for ~p in state ~p", [Info, Call#call.id, State#state.statename]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% handle_hold
%%--------------------------------------------------------------------

handle_hold(GenMediaState, #state{ringuuid = RingUUID, uuid = UUID} = State)
		when is_list(RingUUID) ->
	lager:debug("Calling uuid_phone_event with args ~p", [RingUUID ++ " hold"]),

	case State#state.in_conference of
		true ->
			conference_manager:hold_leg(UUID, ?ORIGINAL_LEG_ID, RingUUID);
		_ ->
			freeswitch:api(State#state.cnode, uuid_phone_event, RingUUID ++ " hold")
	end,

	handle_hold_update(hold, GenMediaState, State).

%%--------------------------------------------------------------------
%% handle_unhold
%%--------------------------------------------------------------------

handle_unhold(GenMediaState, #state{ringuuid = RingUUID, uuid = UUID} = State)
		when is_list(RingUUID) ->
	lager:debug("Calling uuid_phone_event with args ~p", [RingUUID ++ " talk"]),

	case State#state.in_conference of
		true ->
			conference_manager:unhold_leg(UUID, ?ORIGINAL_LEG_ID, RingUUID);
		_ ->
			freeswitch:api(State#state.cnode, uuid_phone_event, RingUUID ++ " talk")
	end,

	handle_hold_update(undefined, GenMediaState, State).

%%--------------------------------------------------------------------
%% handle_hold_update
%%--------------------------------------------------------------------

handle_hold_update(HoldStatus, _GenMediaState, #state{hold = Hold} = State) ->
	Hold2 = case HoldStatus of
		hold -> hold;
		undefined -> undefined;
		_ -> Hold
	end,
	{ok, State#state{hold = Hold2}}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
%% @private
terminate(Reason, _StateName, Call, _Internal, #state{uuid=UUID, in_conference=InConf}) ->
	lager:notice("terminating: ~p ~p", [Reason, Call#call.id]),

	case InConf of
		true ->
			conference_manager:update_leg_state(UUID, ?ORIGINAL_LEG_ID, ended);
		_ ->
			ok
	end,
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, Call, StateName, Sub, Internal, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, _Call, _StateName, Sub, _Internal, _Extra) ->
	{ok, Sub}.

%% Optional gen_media callbacks
is_recording_enabled() ->
	true.

handle_recover(_Call, _Internal, #state{uuid=UUID, cnode=Cnode} = Substate) ->
	gproc:reg({p, g, {freeswitch_node, UUID}}, Cnode),
	{ok, Substate}.

handle_run_ivr(_Call, Args, _Internal, #state{uuid=UUID, cnode=Cnode} = Substate) ->
	Script = proplists:get_value(script, Args),
	lager:info("Calling ~p", ["api luarun " ++ Script ++ " " ++ UUID]),
	freeswitch:api(Cnode, luarun, Script ++ " " ++ UUID),
	{ok, Substate}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @private
fs_send_execute(Node, Callid, _Name, Arg) ->
	freeswitch:sendmsg(Node, Callid, [
		{"call-command", "execute"},
		{"execute-app-name", "set"},
		{"execute-app-arg", Arg}
	]).

originate(Node, BaseDialstring, Destination, InOpts) ->
	{ok, UUID} = freeswitch:api(Node, create_uuid),
	Opts = ["origination_uuid=" ++ UUID | InOpts],
	Dialstring = freeswitch_media_manager:do_dial_string(BaseDialstring, Destination, Opts),
	case freeswitch:bgapi(Node, originate, Dialstring ++ " &park()") of
		{ok, _BgApiId} ->
			case originate_gethandle(Node, UUID) of
				{error, Err} ->
					lager:warning("Cound not originate ~s due to ~p", [UUID, Err]),
					{error, Err};
				_ ->
					{ok, UUID}
			end;
		Bgerr ->
			lager:warning("Could not start originate:  ~p", [Bgerr]),
			{error, Bgerr}
	end.

originate_gethandle(Node, UUID) ->
	originate_gethandle(Node, UUID, 0).

originate_gethandle(Node, UUID, Count) ->
	case freeswitch:handlecall(Node, UUID) of
		{error, badsession} when Count > 10 ->
			{error, badsession};
		{error, badsession} ->
			timer:sleep(100),
			originate_gethandle(Node, UUID, Count + 1);
		{error, Other} ->
			{error, Other};
		Else ->
			Else
	end.

%% @private
case_event_name([UUID | Rawcall], Callrec, State) ->
	Ename = case proplists:get_value("Event-Name", Rawcall) of
		"CUSTOM" -> {"CUSTOM", proplists:get_value("Event-Subclass", Rawcall)};
		Else -> Else
	end,
	%%lager:debug("Event ~p for ~s while in state ~p", [Ename, UUID, State#state.statename]),
	case_event_name(Ename, UUID, Rawcall, Callrec, State).

%% @private
case_event_name({"CUSTOM", "conference::maintenance"}, UUID, _Rawcall, Callrec, #state{statename = Statename, '3rd_party_id' = UUID} = State) when
	Statename =:= 'in_conference'; Statename =:= 'hold_conference' ->
		lager:info("finishing up a 3rd paryt merge",[]),
		case Statename of
			in_conference ->
				#state{cnode = Fnode, conference_id = Confid, ringuuid = Ringid} = State,
				freeswitch:api(Fnode, uuid_transfer, Ringid ++ " 'conference:" ++ Confid ++ "' inline");
			_ ->
				ok
		end,
		cdr:media_custom(Callrec, Statename, ?cdr_states, []),
		{{mediapush, Statename}, State#state{'3rd_party_id' = undefined}};

case_event_name("CHANNEL_ANSWER", UUID, _Rawcall, Callrec, #state{
		statename = hold_conference_3rdparty, '3rd_party_id' = UUID} = State) ->
	#state{cnode = Fnode, ringuuid = Ruuid} = State,
	freeswitch:api(Fnode, uuid_setvar_multi, Ruuid ++ " hangup_after_bridge=false;park_after_bridge=true"),
	freeswitch:bgapi(Fnode, uuid_bridge, UUID ++ " " ++ Ruuid),
	cdr:media_custom(Callrec, '3rd_party', ?cdr_states, []),
	{{mediapush, '3rd_party'}, State#state{statename = '3rd_party'}};

case_event_name("CHANNEL_BRIDGE", UUID, _Rawcall, Callrec, #state{'3rd_party_id' = UUID, statename = '3rd_party'} = State) ->
	lager:debug("Telling agent we're now oncall w/ the 3rd party", []),
	cdr:media_custom(Callrec, '3rd_party', ?cdr_states, []),
	{{mediapush, '3rd_party'}, State};

case_event_name(EventName, UUID, _Rawcall, _Callrec, #state{statename =  blind_transfered} = State) ->
	lager:debug("Blind transfer state doing nothing for event event ~s of uuid ~s", [EventName, UUID]),
	{noreply, State};

case_event_name("CHANNEL_BRIDGE", UUID, _Rawcall, Callrec, #state{ringchannel = Rpid, uuid = UUID} = State) when is_pid(Rpid) ->
	% TODO fix when this can return an {oncall, State}
	RingUUID = freeswitch_ring:get_uuid(State#state.ringchannel),
	SpawnOncall = spawn_monitor(fun() ->
		Oot = gen_media:oncall(Callrec#call.source),
		lager:info("freeswitch_media called gen_media:oncall ~p on channel bridge, result was ~p", [Callrec#call.source, Oot])
	end),
	{noreply, State#state{statename = oncall, spawn_oncall_mon = SpawnOncall, ringuuid = RingUUID}};

case_event_name("CHANNEL_PARK", Thirdy, _Rawcall, Callrec, #state{
	statename = 'in_conference_3rdparty', '3rd_party_id' = Thirdy} = State) ->
		#state{conference_id = Confid, cnode = Fnode, ringuuid = Ringid} = State,
		lager:debug("Most likely in the middle of putting the agent in a conference", []),
	freeswitch:api(Fnode, uuid_transfer, Ringid ++ " 'conference:" ++ Confid ++ "' inline"),
	cdr:media_custom(Callrec, State#state.statename, ?cdr_states, []),
	{{mediapush, 'in_conference_3rdparty'}, State};

case_event_name("CHANNEL_PARK", UUID, _Rawcall, Callrec, #state{statename = hold_conference_3rdparty, '3rd_party_id' = UUID} = State) ->
	lager:debug("park of the 3rd party, proll a hold", []),
	cdr:media_custom(Callrec, State#state.statename, ?cdr_states, []),
	{{mediapush, State#state.statename}, State};

case_event_name("CHANNEL_PARK", UUID, Rawcall, Callrec, #state{
		statename = oncall_hold, uuid = UUID} = State) ->
	Moh = case proplists:get_value("variable_queue_moh", Rawcall, "moh") of
		"silence" ->
			none;
		MohMusak ->
			MohMusak
	end,
	case Moh of
		none ->
			freeswitch:sendmsg(State#state.cnode, UUID,
				[{"call-command", "execute"},
					{"execute-app-name", "playback"},
					{"execute-app-arg", "silence"}]);
		_MohMusak ->
			freeswitch:sendmsg(State#state.cnode, UUID,
				[{"call-command", "execute"},
					{"execute-app-name", "playback"},
					{"execute-app-arg", "local_stream://"++Moh}])
	end,
	cdr:media_custom(Callrec, 'oncall_hold', ?cdr_states, []),
	{{mediapush, caller_hold}, State};

case_event_name("CHANNEL_DESTROY", UUID, _Rawcall, _Callrec, #state{
		'3rd_party_id' = UUID, statename = Statename} = State) ->
	{{mediapush, Statename}, State#state{'3rd_party_id' = undefined}};

case_event_name("CHANNEL_PARK", UUID, Rawcall, Callrec, #state{
		uuid = UUID, queued = false, statename = Statename} = State) when
		Statename == inivr ->
	Queue = proplists:get_value("variable_queue", Rawcall, "default_queue"),
	% Client = {proplists:get_value("variable_brand", Rawcall),[{"dial_vars", State#state.dial_vars} | get_client_options(Rawcall)]},
	Client = proplists:get_value("variable_brand", Rawcall),
	ClientOpts = [{"dial_vars", State#state.dial_vars} | get_client_options(Rawcall)],

	UrlVars = get_url_variables(Rawcall),

	AllowVM = proplists:get_value("variable_allow_voicemail", Rawcall, false),
	Moh = case proplists:get_value("variable_queue_moh", Rawcall, "moh") of
		"silence" ->
			none;
		MohMusak ->
			MohMusak
	end,
	Priority = get_rawcall_int("variable_queue_priority",
		Rawcall, undefined),

	VMPriorityDiff =
		case get_rawcall_int("variable_vm_priority_diff",
				Rawcall, undefined) of
			undefined ->
				ClientRec = Callrec#call.client,
				proplists:get_value(vm_priority_diff, ClientRec#client.options,
				 	?DEFAULT_VM_PRIORITY_DIFF);
			Val ->
				Val
		end,

	Ivropt = proplists:get_value("variable_ivropt", Rawcall),
	SkillList = proplists:get_value("variable_skills", Rawcall, ""),
	Skills = lists:foldl(fun(X, Acc) ->
		try list_to_existing_atom(X) of
			Atom ->
				[Atom | Acc]
		catch
			error:badarg ->
				lager:warning("Freeswitch requested unknown skill ~s~n", [X]),
				Acc
		end
	end, [], util:string_split(SkillList, ",")),

	% {Calleridname, Calleridnum} = get_caller_id(Rawcall),
	CallerId = get_caller_id(Rawcall),
	Doanswer = proplists:get_value("variable_erlang_answer", Rawcall, true),
	CallPs = [
		{client, Client},
		{client_opts, ClientOpts},
		{caller_id, CallerId},
		{priority, Priority},
		{url_vars, UrlVars},
		{skills, Skills}
	],
	% NewCall = Callrec#call{client=Client, callerid={Calleridname, Calleridnum}, priority = Priority, skills = Skills},
	case Doanswer of
		"false" ->
			ok;
		_ ->
			freeswitch:sendmsg(State#state.cnode, UUID,
				[{"call-command", "execute"},
					{"execute-app-name", "answer"}])
	end,
	freeswitch:bgapi(State#state.cnode, uuid_setvar, UUID ++ " hangup_after_bridge true"),
	% play musique d'attente
	case Moh of
		none ->
			freeswitch:sendmsg(State#state.cnode, UUID,
				[{"call-command", "execute"},
					{"execute-app-name", "playback"},
					{"execute-app-arg", "silence"}]);
		_MohMusak ->
			freeswitch:sendmsg(State#state.cnode, UUID,
				[{"call-command", "execute"},
					{"execute-app-name", "playback"},
					{"execute-app-arg", "local_stream://"++Moh}])
	end,
	%% tell gen_media to (finally) queue the media
	freeswitch:api(State#state.cnode, uuid_setvar, UUID ++ " reach_state inqueue"),
	{queue, Queue, CallPs, State#state{queue = Queue, queued=true, allow_voicemail=AllowVM, vm_priority_diff = VMPriorityDiff, moh=Moh, ivroption = Ivropt, statename = inqueue}};

case_event_name("CHANNEL_HANGUP_COMPLETE", UUID, Rawcall, Callrec, #state{uuid = UUID} = State) ->
	lager:debug("Channel hangup ~p", [Callrec#call.id]),
	case State#state.voicemail of
		false -> % no voicemail
			ok;
		FileName ->
			VMSaved = case freeswitch:api(State#state.cnode, file_exists, FileName) of
				{ok, "true"} ->
					true;
				_ ->
					false
			end,

			case VMSaved of
				true ->
					PlaybackMS = proplists:get_value("variable_record_ms", Rawcall),
					PlaybackSamples = proplists:get_value("variable_record_samples", Rawcall),
					PlaybackReadRate = proplists:get_value("variable_read_rate", Rawcall),
					CallerId = get_caller_id(Rawcall),
					Dnis = Callrec#call.dnis,
					Skills = Callrec#call.skills,
					UrlVars = Callrec#call.url_vars,
					Info = [
						{skills, Skills},
						{caller_id, CallerId},
						{dnis, Dnis},
						{playback_ms, PlaybackMS},
						{playback_samples, PlaybackSamples},
						{playback_read_rate, PlaybackReadRate},
						{url_vars, UrlVars}
					],
					lager:notice("~s left a voicemail", [UUID]),
					Client = Callrec#call.client,

					VMPriority = Callrec#call.priority +
						State#state.vm_priority_diff,

					freeswitch_media_manager:new_voicemail(State#state.cnode, UUID, FileName,
						State#state.queue, VMPriority, Client#client.id, Info);
				_ ->
					lager:notice("~s hungup without leaving a voicemail", [UUID])
			end
	end,
%	{hangup, State2};
%"CHANNEL_HANGUP_COMPLETE" ->
	% TODO - this is protocol specific and we only handle SIP right now
	% TODO - this should go in the CDR
	Cause = proplists:get_value("variable_hangup_cause", Rawcall),
	Who = case proplists:get_value("variable_sip_hangup_disposition", Rawcall) of
		"recv_bye" ->
			lager:debug("Caller hungup ~p, cause ~p", [UUID, Cause]),
			"caller";
		"send_bye" ->
			lager:debug("Agent hungup ~p, cause ~p", [UUID, Cause]),
			"agent";
		Other ->
			lager:debug("I don't know who hanged up ~p, cause ~p, hangup disposition ~p ", [UUID, Cause, Other]),
			undefined
		end,
	%{noreply, State};

	Timer = erlang:send_after(?MORATORIUM_TIMEOUT, self(), moratorium_timeout),
	Hupstates = [oncall, oncall_hold, inqueue, inqueue_ringing, inivr],
	Statename = State#state.statename,
	State2 = State#state{agent = undefined, agent_pid = undefined, ringchannel = undefined, moratoriumtimer = Timer},
	case lists:member(Statename, Hupstates) of
		true -> {{hangup, Who}, State2};
		_ -> {noreply, State2}
	end;

case_event_name("CHANNEL_DESTROY", UUID, _Rawcall, Callrec, #state{uuid = UUID} = State) ->
	lager:debug("Last message this will recieve, channel destroy ~p", [Callrec#call.id]),
	{stop, normal, State};

case_event_name("DTMF", UUID, Rawcall, Callrec, #state{allow_voicemail = VmAllowed, queued = true, uuid = UUID} = State) when VmAllowed =/= false ->
	case proplists:get_value("DTMF-Digit", Rawcall) of
		"*" ->
			% allow the media to go to voicemail
			lager:notice("caller requested to go to voicemail ~p", [Callrec#call.id]),
			freeswitch:bgapi(State#state.cnode, uuid_transfer, UUID ++ " 'playback:IVR/prrec.wav,gentones:%(500\\,0\\,500),sleep:600,record:/tmp/${uuid}.wav' inline"),
			case State#state.ringchannel of
				undefined ->
					ok;
				RingChannel ->
					freeswitch_ring:hangup(RingChannel)
			end,
			{voicemail, State#state{voicemail = "/tmp/"++UUID++".wav"}};
		_ ->
			{noreply, State}
	end;

case_event_name({error, notfound}, UUID, Rawcall, _Callrec, State) ->
	lager:warning("event name not found: ~p for ~p", [proplists:get_value("Content-Type", Rawcall), UUID]),
	{noreply, State};

case_event_name("PLAYBACK_STOP", UUID, Rawcall, _, #state{statename = Statename, moh_broadcast = true} = State)
		when Statename =:= inqueue; Statename =:= inqueue_ringing ->
	PlayMoh = proplists:get_value("playback_variable_play_moh_if_inqueue", Rawcall),
	lager:debug("Got PLAYBACK_STOP for ~p while ~p, play moh variable = ~p", [UUID, Statename, PlayMoh]),
	case PlayMoh of
		"true" ->
			freeswitch:api(State#state.cnode, uuid_broadcast, UUID ++ " local_stream://" ++ State#state.moh);
		_ ->
			ok
	end,
	{noreply, State};

case_event_name(Ename, UUID, _, _, #state{statename = Statename} = State) ->
	lager:debug("Event ~p for ~s unhandled while in state ~p", [Ename, UUID, Statename]),
	{noreply, State}.

get_url_variables(Proplist) ->
	lists:foldl(
		fun({"variable_oa_" ++ VarKey, Val}, List) -> List ++ [{list_to_atom(VarKey), Val}];
		(_,List) -> List
		end,
		[],
		Proplist
	).

get_client_options(Proplist) ->
	ExportVars = string:tokens(proplists:get_value("variable_export_vars", Proplist, ""), ","),
	VarNames = ["variable_" ++ V || V <- ExportVars],
	VarValues = [proplists:get_value(N, Proplist, "null") || N <- VarNames,
		proplists:get_value(N, Proplist) =/= undefined],
	VarValues1 = lists:map(fun(Elem) ->
		try ejrpc2_json:decode(Elem) of
			null -> undefined;
			Else when is_binary(Else) -> Else;
			{struct, Else} -> Else;
			Else -> Else
		catch
			error:{case_clause,_} -> Elem;
			error:{badmatch,any} -> undefined
		end
	end, VarValues),
	lists:zip(ExportVars,VarValues1).

get_exported_variables(Proplist) ->
	ExportVars = string:tokens(proplists:get_value("variable_export_vars", Proplist, ""), ","),
	VarNames = ["variable_" ++ V || V <- ExportVars],
	VarValues = [proplists:get_value(N, Proplist, "") || N <- VarNames],
	lists:zipwith(fun (K,V) -> K ++ "=" ++ V end, ExportVars, VarValues).

get_caller_id(Proplist) ->
 InitCallerIdName = proplists:get_value("Caller-Caller-ID-Name", Proplist),
 InitCallerIdNumber = proplists:get_value("Caller-Caller-ID-Number", Proplist, "Unknown"),
 Client = proplists:get_value("variable_brand", Proplist),
 ClientRes = call_queue_config:get_client_by_id(Client),

 InboundCallerId = case ClientRes of
	 none ->
		 InitCallerIdName;
	 {ok, ClientRecord} ->
		 case proplists:get_value(inbound_caller_id, ClientRecord#client.options) of
			 undefined -> InitCallerIdName;
			 Res -> binary_to_list(Res)
		 end
 end,

 lager:debug("Originat cid/cname:  ~p, ~p", [InitCallerIdName, InitCallerIdNumber]),
 {CallerIdName, CallerIdNumber} = case {InboundCallerId, InitCallerIdName, InitCallerIdNumber} of
	 {undefined, NixCidName, _} when NixCidName =:= "unknown"; NixCidName =:= undefined ->
		 {proplists:get_value("variable_sip_from_user_stripped", Proplist, "Unknown"),
		 proplists:get_value("variable_sip_from_uri", Proplist, "Unknown")};
	 {undefined, _, _} ->
		 {InitCallerIdName, InitCallerIdNumber};
	 _ ->
		 {InboundCallerId, InitCallerIdNumber}
 end,
 lager:debug("And what shall be used:  ~p  ~p", [CallerIdName,CallerIdNumber]),
 {freeswitch_util:escape_string(CallerIdName), CallerIdNumber}.

get_info(Cnode, UUID, CustomHeadersNames) ->
	get_info(Cnode, UUID, CustomHeadersNames, 0).

get_info(Cnode, UUID, CustomHeaderNames, Retries) when Retries < 2 ->
	case freeswitch:api(Cnode, uuid_dump, UUID) of
		{ok, Result} ->
			Proplist = lists:foldl(
				fun([], Acc) ->
						Acc;
					(String, Acc) ->
						[Key, Value] = util:string_split(String, ": ", 2),
						[{Key, Value} | Acc]
				end, [], util:string_split(Result, "\n")),

			Priority = try list_to_integer(proplists:get_value("variable_queue_priority", Proplist, "")) of
				Pri -> Pri
			catch
				error:badarg ->
					lager:debug("Cannot get priority from uuid_dump"),
					undefined
			end,
			{CallerIdName, CallerIdNumber} = get_caller_id(Proplist),
				SIPFromDisplay = proplists:get_value("variable_sip_from_display", Proplist, ""),
				SIPFromDisplayEsc = freeswitch_util:escape_string(http_uri:decode(SIPFromDisplay)),
				CustomHeaders = get_custom_headers(CustomHeaderNames, Proplist),
			{proplists:get_value("Caller-Destination-Number", Proplist, ""),
				proplists:get_value("variable_brand", Proplist, ""), Priority,
				CallerIdName, CallerIdNumber,
				SIPFromDisplayEsc,
				get_exported_variables(Proplist),
				CustomHeaders
			};
		timeout ->
			lager:warning("uuid_dump for ~s timed out. Retrying", [UUID]),
			get_info(Cnode, UUID, CustomHeaderNames, Retries + 1);
		{error, Error} ->
			lager:warning("uuid_dump for ~s errored:  ~p. Retrying", [UUID, Error]),
			get_info(Cnode, UUID, CustomHeaderNames, Retries + 1)
	end;
get_info(_, UUID, _CustomHeadersNames, _) ->
	lager:warning("Too many failures doing uuid_dump for ~p", [UUID]),
	{"", "", ?DEFAULT_PRIORITY, "Unknown", "Unknown", "", [], []}.

get_custom_headers(CustomHeadersNames, Proplist) ->
  Headers = ["variable_" ++ H || H <- CustomHeadersNames],
  Values = lists:map(fun(X) -> proplists:get_value(X, Proplist, "") end, Headers),
  lists:zipwith(fun(K, V) -> K ++ "='" ++ V ++ "'" end, CustomHeadersNames, Values).

get_rawcall_int(Key, Rawcall, Default) ->
	case proplists:get_value(Key, Rawcall) of
		undefined ->
			Default;
		ValStr ->
			try list_to_integer(ValStr) of
				ValInt -> ValInt
			catch
				error:badarg ->
					lager:warning("Invalid value for ~s: ~p", [Key, ValStr]),
					Default
			end
	end.

get_recording_path(Call) ->
	CallId = Call#call.id,
	Client = Call#call.client,
	ClientId = Client#client.id,
	Timestamps = Call#call.state_changes,
	InitTime = proplists:get_value(init, Timestamps, os:timestamp()),
	cpx_recordings:get_path(CallId, InitTime, ClientId).

-ifdef(TEST).

hold_media_test_() ->
	{setup, fun() ->
		meck:new(freeswitch),
		meck:expect(freeswitch, api, 3, ok)
	end, fun(_) ->
		meck:unload()
	end, [{"hold media", fun() ->
		Node = 'freeswitch@127.0.0.1',
		RingUUID = "ring-uuid",
		St = #state{cnode = Node, ringuuid = RingUUID, hold = undefined},
		?assertEqual({ok, St#state{hold=hold}}, handle_hold(gm_state, St)),
		?assert(meck:called(freeswitch, api, [Node, uuid_hold, RingUUID]))
	end}, {"unhold media", fun() ->
		Node = 'freeswitch@127.0.0.1',
		RingUUID = "ring-uuid",
		St = #state{cnode = Node, ringuuid = RingUUID, hold = hold},
		?assertEqual({ok, St#state{hold=undefined}}, handle_unhold(gm_state, #state{cnode = Node, ringuuid = RingUUID})),
		?assert(meck:called(freeswitch, api, [Node, uuid_hold, "off " ++ RingUUID]))
	end}]}.

-endif.
