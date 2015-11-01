-module(ouc_agent_info).

%%API
-export([
	get_agent_info/1
]).

-include_lib("reach_core/include/agent.hrl").

get_agent_info(Pid) ->
	Agent = agent:dump_state(Pid),
	#agent_profile{name = Profile, skills = Skills} = try agent_auth:get_profile(Agent#agent.profile) of
		{ok, P} ->
			P;
		_ ->
			lager:warning("Agent ~p has an invalid profile of ~p, using Default",
				[Agent#agent.login, Agent#agent.profile]),
			agent_auth:get_default_profile()
	catch
		error:{case_clause, {aborted, _}} ->
			#agent_profile{name = error}
	end,
	{FirstName, LastName} = get_agent_name(Agent#agent.login),
	ProfSkills = expand_magic_skills(Agent, Skills),
	% InherentSkills = expand_magic_skills(Agent, Agent#agent.skills),
	% MergedSkills = util:merge_skill_lists(ProfSkills, InherentSkills,
	% 					['_queue', '_brand']),
	MergedSkills = expand_magic_skills(Agent, Agent#agent.skills),
	InherentSkills = MergedSkills -- ProfSkills,
	AgentInfo = #agent_info{login = Agent#agent.login, id = Agent#agent.id,
						skills = MergedSkills, inherentskills = InherentSkills,
						firstname = FirstName, lastname = LastName,
						profile = Profile, profileskills = ProfSkills},
	AgentInfo.



get_agent_name(Login) ->
	case agent_auth:get_agent(Login) of
		{ok, AgentAuth} -> {AgentAuth#agent_auth.firstname,
							AgentAuth#agent_auth.lastname};
		_ -> {undefined, undefined}
	end.

expand_magic_skills(State, Skills) ->
	lists:map(
		fun('_agent') -> {'_agent', State#agent.login};
		('_node') -> {'_node', node()};
		('_profile') -> {'_profile', State#agent.profile};
		(Skill) -> Skill
	end, Skills).