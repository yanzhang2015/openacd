%% Copyright (c) 2013 eZuce, Inc. All rights reserved.
%% Contributed to SIPfoundry under a Contributor Agreement
%%
%% This software is free software; you can redistribute it and/or modify it under
%% the terms of the Affero General Public License (AGPL) as published by the
%% Free Software Foundation; either version 3 of the License, or (at your option)
%% any later version.
%%
%% This software is distributed in the hope that it will be useful, but WITHOUT
%% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
%% FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
%% details.

-module(oacd_dialplan_listener).

-behaviour(gen_server).

-include("oadp_internal.hrl").

-include_lib("reach_core/include/call.hrl").
-include_lib("reach_core/include/agent.hrl").

%% api
-export([
	start/1,
	start_link/1,
	stop/0,
	get_timeout/0,
	set_timeout/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	timeout_ms :: non_neg_integer()
}).

-define(DEFAULT_ANNOUNCEMENT, element(2, application:get_env(reach_dialplan, agent_login_announcement))).

%% api

start(Args) ->
	gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
	gen_server:call(?MODULE, stop).

get_timeout() ->
	gen_server:call(?MODULE, get_timeout).

set_timeout(TMs) ->
	gen_server:cast(?MODULE, {set_timeout, TMs}).

%% gen_server callbacks

init(Args) ->
	TMs = p_as_timeout_ms(proplists:get_value(timeout_ms, Args, ?DEFAULT_TIMEOUT_MS)),
	lager:info("Starting oacd_dialplan_listener with timeout_ms: ~p", [TMs]),
	{ok, #state{timeout_ms=TMs}}.

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(get_timeout, _From, State) ->
	{reply, State#state.timeout_ms, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({set_timeout, TMs}, State) ->
	{noreply, State#state{timeout_ms=p_as_timeout_ms(TMs)}};
handle_cast(_Msg, State) ->
	{noreply, State}.


handle_info({freeswitch_sendmsg, "agent_pstn_login " ++ Args}, State) ->
  lager:info("Login using PSTN request from fs: ~p", [Args]),
  case string:tokens(Args, " ") of
    [FNode, Uuid, AgentLogin, AgentPass, AgentPstn] ->
      PstnURI = fix_pstn_line(AgentPstn),
      Endpoints = get_endpoints(PstnURI),
      case agent_manager:start_agent_by_pstn_login(AgentLogin, AgentPass, Endpoints) of
        {ok, P} ->
          {ok, Conn} = oadp_conn:start([{timeout_ms, State#state.timeout_ms}]),
          agent:set_connection(P, Conn),
          agent:go_available(P);
        {exists, P} ->
          agent:go_available(P);
        {error, not_allowed} ->
          play_announcement(FNode, Uuid, ?DEFAULT_ANNOUNCEMENT),
          lager:warning("The number of concurrent users exceeded license limit", []),
          ok;
        _ ->
          lager:warning("Invalid credentials for agent ~p", [AgentLogin]),
          ok
      end;
    _ ->
      lager:warning("Invalid arguments in agent_pstn_login: ~p", [Args]),
      ok
  end,
  {noreply, State};
handle_info({freeswitch_sendmsg, "agent_pstn_logout " ++ AgentLogin}, State) ->
  lager:info("Logout using PSTN request from fs: ~p", [AgentLogin]),
  case agent_manager:query_agent(AgentLogin) of
    {true, P} ->
      agent:stop(P);
    _ ->
      ok
  end,
  {noreply, State};
handle_info({freeswitch_sendmsg, "agent_pstn_go_available " ++ AgentLogin}, State) ->
  lager:info("Go available using PSTN request from fs: ~p", [AgentLogin]),
  case agent_manager:query_agent(AgentLogin) of
    {true, P} ->
      agent:go_available(P);
    _ ->
      ok
  end,
  {noreply, State};
handle_info({freeswitch_sendmsg, "agent_pstn_go_released " ++ AgentLogin}, State) ->
  lager:info("Go released request from fs: ~p", [AgentLogin]),
  case agent_manager:query_agent(AgentLogin) of
    {true, P} ->
      agent:go_released(P);
    _ ->
      ok
  end,
  {noreply, State};

handle_info({freeswitch_sendmsg, "agent_login " ++ Args}, State) ->
	lager:info("Login request from fs: ~p", [Args]),
	case string:tokens(Args, " ") of
		[FNode, Uuid, Login] ->
			case agent_manager:start_agent_by_login(Login) of
				{ok, P} ->
					{ok, Conn} = oadp_conn:start([{timeout_ms, State#state.timeout_ms}]),
					agent:set_connection(P, Conn),
					agent:go_available(P);
				{exists, P} ->
					agent:go_available(P);
				{error, not_allowed} ->
					play_announcement(FNode, Uuid, ?DEFAULT_ANNOUNCEMENT),
					lager:warning("The number of concurrent users exceeded license limit", []),
					ok;
				_ ->
					ok
			end;
		_ ->
			lager:warning("Cannot login agent using dialstring, invalid arguments", []),
			ok
	end,
	{noreply, State};
handle_info({freeswitch_sendmsg, "agent_logout " ++ Login}, State) ->
	lager:info("Logout request from fs: ~p", [Login]),
	case agent_manager:query_agent(Login) of
		{true, P} ->
			agent:stop(P);
		_ ->
			ok
	end,
	{noreply, State};
handle_info({freeswitch_sendmsg, "agent_go_available " ++ Login}, State) ->
	lager:info("Go available request from fs: ~p", [Login]),
	case agent_manager:query_agent(Login) of
		{true, P} ->
			case agent:dump_state(P) of
				Agent when is_record(Agent, agent) ->
					%% Check if the agent has previous used channels and end wrapup in that case
					UsedChannels = dict:to_list(Agent#agent.used_channels),
					Res = [K || {K, V} <- UsedChannels, V =:= voice],
					case Res of
						[ChanPid] when is_pid(ChanPid) ->
							lager:info("Ending wrapup for ~p", [ChanPid]),
							agent_channel:end_wrapup(ChanPid);
						_ ->
							ok
					end;
				_ ->
					lager:warning("Cannot get details about the agent ~p state", [Login]),
					ok
			end,
			agent:go_available(P);
		_ ->
			ok
	end,
	{noreply, State};
handle_info({freeswitch_sendmsg, "agent_go_released " ++ Login}, State) ->
	lager:info("Go released request from fs: ~p", [Login]),
	case agent_manager:query_agent(Login) of
		{true, P} ->
			agent:go_released(P);
		_ ->
			ok
	end,
	{noreply, State};

%% --------------------------------------------------------------------
%% @doc Handles set_call_skills message. This message can be sent
%%  from an IVR executed from a recipe step, in order to change the
%%  skills for a call identified by a uuid.
%%      This method checks if there is a freeswitch_media pid that
%%  handles the call and if so, gets the vqueue pid where the call
%%  was initially queued. Skills update is handled then by the
%%  reach_vqueue_manager that will ensure a new vqueue is created
%%  with the given skills if none already exists.
%%      Arguments should have the format:
%%        "<UUID> <InitialQueue> <NewSkills>", where:
%%            UUID: session:getVariable("uuid");
%%            InitialQueue: session:getVariable("queue");
%%            NewSkills: list of comma separated skills,
%%              whithout whitespaces! Possible magic skills:
%%              _queue, _brand, _node.
%% @end
%% --------------------------------------------------------------------
handle_info({freeswitch_sendmsg, "set_call_skills " ++ Arguments}, State) ->
  lager:info("Setting call skills from lua script with arguments: ~p", [Arguments]),
  case string:tokens(Arguments, " ") of
    [CallId, QName, Skills] ->
      SkList = string:tokens(Skills, ","),
      SkListAtoms = [list_to_atom(S) || S <- SkList ],
      case freeswitch_media_manager:get_handler(CallId) of
        Pid when is_pid(Pid) ->
          case freeswitch_media_manager:get_media(Pid) of
            {_, CallPid} ->
              QPid = reach_vqueue_manager:get_vqueue_pid(QName, CallId),
              if QPid =/= undefined ->
                reach_vqueue_manager:set_skills(QName, QPid, CallPid, SkListAtoms),
                ok;
              true ->
                lager:warning("No vqueue exists for the call ", []),
                ok end;
            _ ->
              lager:warning("No media pid associated with the call ~p", [CallId]),
              ok
          end;
        _ ->
          lager:warning("No freeswitch media handler for call ~p", [CallId]),
          ok
      end,
      ok;
    _ ->
      lager:warning("Invalid arguments sent from lua script, skipping...", []),
      ok
  end,
  {noreply, State};

handle_info({freeswitch_sendmsg, "go_to_voicemail " ++ CallId}, State) ->
  lager:notice("Request to go to voicemail from a lua script with arguments: go_to_voicemail ~p", [CallId]),
  case freeswitch_media_manager:get_handler(CallId) of
    Pid when is_pid(Pid) ->
      case gen_media:try_voicemail(Pid) of
        ok ->
          lager:info("Send to voicemail successful for call ~p ", [Pid]);
        deferred ->
          lager:info("Send to voicemail is deferred");
        Els ->
          lager:warning("Send to voicemail returned: ~p", [Els])
      end;
    _ ->
      lager:warning("No freeswitch media pid associated with the call id ~p", [CallId])
  end,
  {noreply, State};

handle_info(Msg, State) ->
	lager:debug("Unexpected msg: ~p", [Msg]),
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% internal

p_as_timeout_ms(N) when is_integer(N), N > 0 -> N;
p_as_timeout_ms(_) -> none.


fix_pstn_line(PstnLine) ->
  Addr = freeswitch_media_manager:fix_sip_addr(PstnLine),
  Addr.


get_endpoints(SipURI) ->
  Endpoints = [{freeswitch_media,
    [{type,pstn},{data, SipURI}]},
    {freeswitch_voicemail,
      [{type,pstn},{data, SipURI}]},
    {freeswitch_conference,
      [{type,pstn},{data, SipURI}]}],
  Endpoints.

play_announcement(FNode, Uuid, Announcement) ->
  freeswitch:sendmsg(list_to_atom(FNode), Uuid,
    [{"call-command", "execute"},
      {"execute-app-name", "playback"},
      {"execute-app-arg", Announcement}]).
