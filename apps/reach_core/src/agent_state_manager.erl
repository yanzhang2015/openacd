-module(agent_state_manager).

-include("agent.hrl").

-define(AGENT_PROP, {p, g, cpx_online_agent}).

-export([
	init/2,
	set_release_state/3,
	set_channel_state/4,
	get_channel/2,
	set_offline/2,
	list/0,
	subscribe/1
]).

init(Agent, ReleaseState) ->
	UState = case ReleaseState of
		available -> idle;
		_ -> released
	end,
	#agent{
		login = Login,
		profile = Profile,
		firstname = FirstName,
		lastname = LastName} = Agent,
	AgentState = #cpx_agent_state{
		login = Login,
		profile = Profile,
		firstname = FirstName,
		lastname = LastName,
		ustate = UState,
		rstate = ReleaseState
	},
	gproc:reg(?AGENT_PROP, AgentState),
	gproc:send({p, g, cpx_agent_ustate_change},
				{cpx_agent_ustate_change, offline, UState, AgentState}),
	AgentState.

set_release_state(_Agent, ReleaseState, AgentState) ->
	UState = AgentState#cpx_agent_state.ustate,
	UState2 = case {ReleaseState, UState} of
		{released, idle} -> released;
		{available, released} -> idle;
		_ -> UState
	end,
	AgentState2 = AgentState#cpx_agent_state{
		ustate = UState2,
		rstate = ReleaseState
	},
	gproc:set_value(?AGENT_PROP, AgentState2),
	case UState =/= UState2 of
		true ->
			gproc:send({p, g, cpx_agent_ustate_change},
				{cpx_agent_ustate_change, UState, UState2, AgentState2});
		_ ->
			ok
	end,
	AgentState2.

set_channel_state(_Agent, _Channel, ChannelState, AgentState) ->
	ReleaseState = AgentState#cpx_agent_state.rstate,
	UState = AgentState#cpx_agent_state.ustate,
	UState2 = case {ChannelState, ReleaseState} of
		{undefined, available} -> idle;
		{undefined, released} -> released;
		_ -> ChannelState
	end,
	AgentState2 = AgentState#cpx_agent_state{
		ustate = UState2
	},
	gproc:set_value(?AGENT_PROP, AgentState2),
	case UState =/= UState2 of
		true ->
			gproc:send({p, g, cpx_agent_ustate_change},
				{cpx_agent_ustate_change, UState, UState2, AgentState2});
		_ ->
			ok
	end,
	AgentState2.

set_offline(_Agent, AgentState) ->
	UState = AgentState#cpx_agent_state.ustate,
	gproc:send({p, g, cpx_agent_offline}, {cpx_agent_offline, UState, AgentState}).

get_channel(Agent, ChannelId) ->
	gproc:where({n, g, {agent_channel, Agent, ChannelId}}).

list() ->
	gproc:lookup_values(?AGENT_PROP).

subscribe(ustate) ->
	catch gproc:add_global_property(cpx_agent_offline, subscribe),
	case catch gproc:add_global_property(cpx_agent_ustate_change, subscribe) of
		true -> true;
		_ -> false
	end.
