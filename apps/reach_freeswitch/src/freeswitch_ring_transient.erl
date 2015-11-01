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

%% @doc A transient ring channel.

-module(freeswitch_ring_transient).

-include_lib("reach_core/include/call.hrl").
-include_lib("reach_core/include/agent.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
	init/2,
	handle_event/4,
	handle_call/4,
	handle_cast/3,
	handle_info/3,
	terminate/3,
	code_change/3
]).

-record(state, {
	call :: 'undefined' | #call{},
	no_oncall_on_bridge :: 'undefined' | 'true',
	hold :: 'hold' | 'undefined'
}).

%% ======
%% API
%% ======

%  Yes it's blank for now.

%% =====
%% freeswitch_ring callbacks
%% =====

%% =====
%% init
%% =====
init(_Fsref, Options) ->
	Call = proplists:get_value(call, Options),
	Agent = proplists:get_value(agent, Options),
	Chan = proplists:get_value(agent_channel_pid, Options),
	case Call of
		undefined -> ok;
		_ when is_record(Call, call) ->
			gen_media:takeover_ring(Call#call.source, {Agent#agent.login, Chan})
	end,
	{ok, [], #state{
		call = proplists:get_value(call, Options),
		no_oncall_on_bridge = proplists:get_value(no_oncall_on_bridge, Options)
	}}.

%% =====
%% handle_call
%% =====
handle_call(_Msg, _From, _FsRef, State) ->
	{reply, invalid, State}.

%% =====
%% handle_cast
%% =====
handle_cast(hangup, {FsNode, UUID}, State) ->
	freeswitch:bgapi(FsNode, uuid_kill, UUID),
	{stop, normal, State};
handle_cast({agent_state, oncall, #call{type = IsVoice}}, _FsRef, State) when IsVoice =:= voice; IsVoice =:= voicemail ->
	% bridging will happen, and all will be happy.
	{noreply, State};
handle_cast({agent_state, AState, _Data}, FsRef, State) ->
	handle_cast({agent_state, AState}, FsRef, State);
handle_cast({agent_state, _AState}, {FsNode, UUID}, State) ->
	% live fast, die young, leave a beautiful exit message.
	freeswitch:bgapi(FsNode, uuid_kill, UUID),
	{stop, normal, State};
handle_cast(_Msg, _FsRef, State) ->
	{noreply, State}.

%% =====
%% handle_info
%% =====
handle_info({cpx_endpoint, hangup}, {FsNode, UUID}, State) ->
	freeswitch:sendmsg(FsNode, UUID,
		[{"call-command", "hangup"}]),
	{stop, normal, State};
handle_info({stop, Reason}, _FsRef, State) ->
	{stop, Reason, State};
handle_info(_Msg, _FsRef, State) ->
	{noreply, State}.

%% =====
%% handle_event
%% =====
handle_event("CHANNEL_ANSWER", _Data, _FsRef, #state{call = undefined} = State) ->
	{noreply, State};
handle_event("CHANNEL_ANSWER", _Data, {FSNode, _UUID}, #state{call = #call{type = IsVoice} = Call} = State) when IsVoice =:= voice; IsVoice =:= voicemail ->
	%% the freeswitch media will ask self() for some info,
	%% so the needs to be spawned out.
	Self = self(),
	Fun = fun() ->
		try gen_media:oncall(Call#call.source) of
			invalid ->
				lager:info("freeswitch_ring_transient spawned gen_media:oncall ~p on channel answer, result was ~p", [Call#call.source, invalid]),
				freeswitch:api(FSNode, uuid_park, Call#call.id),
				lager:debug("Death due to invalid oncall request", []),
				Self ! {stop, normal};
			{error, invalid} ->
				lager:info("freeswitch_ring_transient spawned gen_media:oncall ~p on channel answer, result was ~p", [Call#call.source, invalid]),
				freeswitch:api(FSNode, uuid_park, Call#call.id),
				lager:debug("Death due to invalid oncall request", []),
				Self ! {stop, normal};
			{error, Err} ->
				lager:error("freeswitch_ring_transient spawned gen_media:oncall ~p on channel answer, result was ~p", [Call#call.source, Err]),
				freeswitch:api(FSNode, uuid_park, Call#call.id),
				Self ! {stop, normal};
			ok ->
				lager:info("freeswitch_ring_transient spawned gen_media:oncall ~p on channel answer, result was ~p", [Call#call.source, ok]),
				ok
		catch
			exit:{noproc, _} ->
				lager:warning("~p died before I could complete the bridge", [Call#call.source]),
				freeswitch:api(FSNode, uuid_park, Call#call.id),
				lager:debug("Death due to noproc error on oncall attempt", []),
				Self ! {stop, normal}
		end
	end,
	spawn(Fun),
	{noreply, State};
handle_event("CHANNEL_ANSWER", _Data, {FsNode, UUID}, #state{call = Call} = State) ->
	% Ah, this is not a freeswitch call.  If the oncall works, I can die.
	try gen_media:oncall(Call#call.source) of
		invalid ->
			lager:info("freeswitch_ring_transient called gen_media:oncall ~p on channel answer, result was ~p", [Call#call.source, invalid]),
			{noreply, State};
		ok ->
			lager:info("freeswitch_ring_transient called gen_media:oncall ~p on channel answer, result was ~p", [Call#call.source, ok]),
			freeswitch:api(FsNode, uuid_kill, UUID),
			lager:debug("Death due to non-voice call answered", []),
			{stop, normal, State}
	catch
		exit:{noproc, _} ->
			lager:warning("~p died before I could complete the bridge, killing ~p", [Call#call.source, UUID]),
			freeswitch:api(FsNode, uuid_kill, UUID),
			lager:debug("Death due to noproc error setting non-voice call oncall", []),
			{stop, normal, State}
	end;
handle_event("CHANNEL_BRIDGE", _Data, _FsRef, #state{no_oncall_on_bridge = true} = State) ->
	{noreply, State};
handle_event("CHANNEL_BRIDGE", _Data, {Fsnode, _UUID}, #state{call = #call{type = voice} = Call} = State) ->
	try gen_media:oncall(Call#call.source) of
		invalid ->
			lager:info("freeswitch_ring_transient called gen_media:oncall ~p on channel bridge, result was ~p", [Call#call.source, invalid]),
			freeswitch:api(Fsnode, uuid_park, Call#call.id),
			lager:debug("Death due to invalid oncall request after bridge", []),
			{stop, normal, State};
		ok ->
			lager:info("freeswitch_ring_transient called gen_media:oncall ~p on channel bridge, result was ~p", [Call#call.source, ok]),
			{noreply, State}
	catch
		exit:{noproc, _} ->
			lager:warning("~p died before I could complete the bridge, I die with it", [Call#call.source]),
			freeswitch:api(Fsnode, uuid_park, Call#call.id),
			{stop, normal, State}
	end;

handle_event("CHANNEL_CALLSTATE", Data, _FsRef, #state{call = Call} = State) ->
	CallState = proplists:get_value("Channel-Call-State", Data),
	PrevCallState = proplists:get_value("Original-Channel-Call-State", Data),
	lager:info("Received CHANNEL_CALLSTATE change event from ~p to ~p", [PrevCallState, CallState]),
	case {CallState, PrevCallState} of
		{"UNHELD", "HELD"} -> gen_media:hold_update(Call#call.source, undefined);
		{"HELD", _} -> gen_media:hold_update(Call#call.source, hold);
		_ -> ok
	end,
	{noreply, State};

handle_event("CHANNEL_HANGUP", Data, _Fsref, State) ->
	HangupCause = proplists:get_value("Hangup-Cause", Data),
	SipStatus = proplists:get_value("variable_sip_term_status", Data),
	SipCause = proplists:get_value("variable_sip_term_cause", Data),
	StopReason = case {HangupCause, SipStatus, SipCause} of
		{"RECOVERY_ON_TIMER_EXPIRE", "408", "102"} -> call_expired;
		{"NO_ANSWER", _, _} -> {shutdown, call_expired};
        {"USER_BUSY", _, _} -> {shutdown, call_expired};
		_ -> normal
    %% @TODO Add more hangup reasons using codes from https://freeswitch.org/confluence/display/FREESWITCH/Hangup+Cause+Code+Table
	end,
	lager:info("Channel hangup event with cause ~p; stopping with reason ~p", [[HangupCause, SipStatus, SipCause], StopReason]),
	{stop, StopReason, State};

handle_event("PLAYBACK_STOP", _Data, _Fsref, #state{call = #call{source = CallPid} = Call} = State) ->
	CallPid ! {event, playback_stop, Call},
	{noreply, State};

handle_event(Event, _, _, State) ->
	lager:debug("Ignoring event ~p", [Event]),
	{noreply, State}.

%% =====
%% terminate
%% =====

terminate(Reason, _Fsref, _State) ->
	lager:notice("Going down:  ~p", [Reason]),
	ok.

%% =====
%% code_change
%% =====

code_change(_oldVsn, State, _Extra) ->
	{ok, State}.

%% =====
%% internal api
%% =====
