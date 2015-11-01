-module(ouc_test_util).

-include_lib("reach_core/include/agent.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
	agent/0,
	agent/1,
	agent/2,

	props_test_/1,
	props_test_/2
]).

agent() ->
	agent("agent").

agent(Login) ->
	agent(Login, "Default").

agent(Login, Profile) ->
	%% silence sasl
	error_logger:delete_report_handler(error_logger_tty_h),

	application:start(gproc),
	cpx_hooks:start_link(),
	agent_auth:start(),
	cpx_agent_event:start(),

	Agent = #agent{id=Login, login=Login, profile=Profile},
	{ok, APid} = agent:start_link(Agent, []),
	APid.

props_test_(Mod) ->
	props_test_(Mod, []).

props_test_(Mod, Opts) ->
	case os:getenv("PBT") of
		"1" ->
			PropsMod = list_to_atom(atom_to_list(Mod) ++ "_props"),
			[{atom_to_list(X), {timeout, 60, ?_assert(proper:quickcheck(PropsMod:X(), Opts))}} ||
				{X, 0} <- PropsMod:module_info(exports), X > 'prop_', X < 'prop`'];
		_ ->
			[]
	end.
