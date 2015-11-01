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

-module(oadp_conn).

-behaviour(gen_server).

-include("oadp_internal.hrl").

%% api
-export([
	start/0,
	start/1,
	stop/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	timeout_ms :: integer(),
	tref :: reference()
}).

%% api

start() ->
	start([]).

start(Opts) ->
	gen_server:start(?MODULE, Opts, []).

stop(Pid) ->
	gen_server:call(Pid, stop).

%% gen_server callbacks

init(Opts) ->
	TMs = proplists:get_value(timeout_ms, Opts, ?DEFAULT_TIMEOUT_MS),

	St = #state{timeout_ms=TMs},
	St1 = p_reset_timer(St),

	gproc:add_global_property(?MODULE, undefined),

	{ok, St1}.

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

%% Reset timer on set_release
handle_info({agent, {set_release, _, _}}, St) ->
	{noreply, p_reset_timer(St)};

%% Reset timer on set_channel
%% Auto wrap-up calls
handle_info({agent, {set_channel, Pid, _ChanId, wrapup, _Call}}, St) ->
	lager:info("Auto wrapping up: ~p", [Pid]),
	%%agent_channel:end_wrapup(Pid),
	{noreply, p_reset_timer(St)};
handle_info({agent, {set_channel, _, _, ringing, _}}, St) ->
	{noreply, p_cancel_timer(St)};
handle_info({agent, {set_channel, _, _, oncall, _}}, St) ->
	{noreply, St};
handle_info({agent, {set_channel, _, _, _, _}}, St) ->
	{noreply, p_reset_timer(St)};

handle_info({agent, {channel_died, _, _, _}}, St) ->
	{noreply, p_reset_timer(St)};

handle_info({timeout, TRef, _}, State=#state{tref=TRef}) ->
	lager:info("Timed out, dying..."),
	{stop, normal, State};

handle_info(_Msg, State) ->
	% lager:debug("Unexpected msg: ~p", [_Msg]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% internal

p_reset_timer(St=#state{tref=TRef0, timeout_ms=TMs}) when is_integer(TMs), TMs > 0 ->
	catch erlang:cancel_timer(TRef0),
	TRef = erlang:start_timer(TMs, self(), timeout),
	St#state{tref=TRef};
p_reset_timer(St) ->
	St.

p_cancel_timer(St=#state{tref=TRef})->
	catch erlang:cancel_timer(TRef),
	St#state{tref=undefined}.
