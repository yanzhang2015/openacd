-module(ouc_agent_event_handler).
-behaviour(gen_event).

-include_lib("reach_core/include/agent.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([get_agent_listener/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
	handle_info/2, code_change/3, terminate/2]).

%% API

-spec get_agent_listener(AgentId :: string()) -> pid() | undefined.
get_agent_listener(AgentId) ->
	gproc:where({n, g, {ouc_agent_listener, AgentId}}).

%% gen_event callbacks

init([]) ->
	{ok, undefined}.

handle_event({agent_init, Agent, Now}, State) ->
	ouc_agent_listener:start_link([Agent, Now]),
	{ok, State};
handle_event(Event = {change_release_state, AgentId, _, _}, State) ->
	send_event(AgentId, Event),
	{ok, State};
handle_event(Event = {channel_init, Agent, _, _, _, _}, State) ->
	AgentId = Agent#agent.id,
	send_event(AgentId, Event),
	{ok, State};
handle_event(Event = {channel_change, Agent, _, _, _, _}, State) ->
	AgentId = Agent#agent.id,
	send_event(AgentId, Event),
	{ok, State};
handle_event(_Event, _State) ->
	lager:debug("~p got unhandled event ~p", [?MODULE, _Event]),
	{ok, _State}.

handle_call(_Request, _State) ->
	{ok, _State}.

handle_info(_Info, _State) ->
	{ok, _State}.

terminate(_Arg, _State) ->
	ok.

code_change(_OldVsn, _State, _Extra) ->
	{ok, state}.

%% internal functions

send_event(AgentId, Event) ->
	ServerName = {ouc_agent_listener, AgentId},
	catch gproc:send({n, g, ServerName}, Event).

%% eunit tests

-ifdef(TEST).

t_event(Event) ->
	?assertEqual({ok, state}, handle_event(Event, state)).

handle_event_test_() ->
	{setup, fun() ->
		application:start(gproc),
		meck:new(ouc_agent_listener),
		meck:expect(ouc_agent_listener, start_link, 1, ok)
	end, fun(_) ->
		meck:unload()
	end, [
	{"agent_init", fun() ->
		t_event({agent_init, agent, 130000}),
		?assert(meck:called(ouc_agent_listener, start_link, [[agent, 130000]]))
	end},
	{"change_release_state", fun() ->
		t_event({change_release_state, "openacdagent", released, 130000})
	end},
	{"channel_init", fun() ->
		t_event({channel_init, #agent{id="openacdagent", login="agent"},
			pid, ringing, call, 130000})
	end},
	{"channel_change", fun() ->
		t_event({channel_change, #agent{id="openacdagent", login="agent"},
			pid, oncall, call, 130000})
	end}]}.

-endif.
