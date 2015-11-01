-module(ouc_sup_rpc).
-author("jvliwanag@ezuce.com").
-export([get_agent_profiles/1,
	subscribe_agent_profiles/1,
	get_agents/1,
	subscribe_agents/1,
	kick_agent/2,
	get_queues/1,
	subscribe_queues/1,
	get_queued_calls/1,
	subscribe_queued_calls/1,
	set_agent_state/3,
	monitor_call/3,
	barge_call/3,
	get_recordings/5
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-import(cpx_json_util, [l2b/1, b2l/1, nob/1]).

-include("oacd_ouc.hrl").
-include("ouc_rstat.hrl").
-include("ouc_recordings.hrl").

-include_lib("reach_core/include/agent.hrl").
-include_lib("reach_core/include/call.hrl").
-include_lib("reach_core/include/queue.hrl").
-include_lib("erlmongo/include/erlmongo.hrl").
-include_lib("reach_core/include/gen_media.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% @doc Return the agent profiles. A profile contains a name, skills, counts and statistics.
%% === Arguments: ===
%% ```none (internal state) '''
%% === Answer: ===
%% ```
%%  {
%%    "profiles": [
%%      {
%%        "name": "Default",
%%        "skills": [],
%%        "counts": {
%%          "total": 2,
%%          "released": 1,
%%          "idle": 1,
%%          "ringing": 0,
%%          "oncall": 0,
%%          "wrapup": 0
%%        },
%%        "rstats": [
%%          {
%%            "coverage": "last_15m",
%%            "occupancy": 26.505016722408026,
%%            "cpt": 158.5,
%%            "calls": 2,
%%            "abandoned_calls": 0,
%%            "average_abandon_duration": "infinity",
%%            "average_wait_duration": 2.0,
%%            "max_wait_duration": 3
%%          },
%%          {
%%            "coverage": "last_30m",
%%            "occupancy": 26.505016722408026,
%%            "cpt": 158.5,
%%            "calls": 2,
%%            "abandoned_calls": 0,
%%            "average_abandon_duration": "infinity",
%%            "average_wait_duration": 2.0,
%%            "max_wait_duration": 3
%%          },
%%          {
%%            "coverage": "last_hr",
%%            "occupancy": 26.505016722408026,
%%            "cpt": 158.5,
%%            "calls": 2,
%%            "abandoned_calls": 0,
%%            "average_abandon_duration": "infinity",
%%            "average_wait_duration": 2.0,
%%            "max_wait_duration": 3
%%          },
%%          {
%%            "coverage": "today",
%%            "occupancy": 24.44101773323053,
%%            "cpt": 158.5,
%%            "calls": 2,
%%            "abandoned_calls": 0,
%%            "average_abandon_duration": "infinity",
%%            "average_wait_duration": 2.0,
%%            "max_wait_duration": 3
%%          },
%%          {
%%            "coverage": "this_week",
%%            "occupancy": 24.44101773323053,
%%            "cpt": 158.5,
%%            "calls": 2,
%%            "abandoned_calls": 0,
%%            "average_abandon_duration": "infinity",
%%            "average_wait_duration": 2.0,
%%            "max_wait_duration": 3
%%          },
%%          {
%%            "coverage": "this_month",
%%            "occupancy": 24.44101773323053,
%%            "cpt": 158.5,
%%            "calls": 2,
%%            "abandoned_calls": 0,
%%            "average_abandon_duration": "infinity",
%%            "average_wait_duration": 2.0,
%%            "max_wait_duration": 3
%%          }
%%        ]
%%      }
%%    ]
%%  }'''
%% @end
get_agent_profiles(_St) ->
	ProfileStats = ouc_agent_profiles:get_profiles(),
	{ok, Profiles} = agent_auth:get_profiles(),
	ProfileSkills = [{Name, Skills} || #agent_profile{name = Name, skills = Skills} <- Profiles],
	GetSkills = fun(P) -> proplists:get_value(P, ProfileSkills) end,

	PRstats = ouc_rstat:get_profile_rstats(),

	CountsArr = [{[
		{name, l2b(Name)},
		{skills, cpx_json_util:enc_skills(GetSkills(Name))},
		{counts, counts_to_entries(Pc)},
		{rstats, rstats_to_entries(PRstats, [{profile, Name}])}
	]} || {Name, Pc} <- ProfileStats],
	{[{profiles, CountsArr}]}.


%% @doc Subscribe to agent profile changes.
%% === Arguments: ===
%% ```none (internal state) '''
%% === Answer: ===
%% ```Same as get_agent_profiles.'''
%% @see ouc_sup_rpc:get_agent_profiles/1
%% @end
subscribe_agent_profiles(St) ->
	ouc_agent_profiles:subscribe(),
	ouc_rstat:subscribe(),

	%% hmm, this is a nasty way of setting state
	%% see info handling on ouc_wsock_handler
	self() ! {ouc_set_agent_profiles_subscribed, true},

	get_agent_profiles(St).

%% @doc Returns a list of details for all online agents.
%% === Arguments: ===
%% ```none (internal state) '''
%% === Answer: ===
%% ```
%%  {
%%    "agents": [
%%      {
%%        "username": "1234",
%%        "profile": "Default",
%%        "login_time": "1409295538912",
%%        "first_name": "Bob",
%%        "last_name": "Smith",
%%        "reach_node": "ucdev.ezuce.ph",
%%        "location": null,
%%        "job_title": null,
%%        "skills": [
%%          "english",
%%          "german",
%%          {
%%            "agent": "1234"
%%          },
%%          {
%%            "client": "client name"
%%          },
%%          {
%%            "node": "reach@ucdev.ezuce.ph"
%%          },
%%          {
%%            "profile": "Default"
%%          }
%%        ],
%%        "avatar": "https://ucdev.ezuce.ph/sipxconfig/rest/avatar/1234",
%%        "state": "available",
%%        "active_channels": [],
%%        "rstats": {
%%          "coverages": [
%%            {
%%              "coverage": "last_15m",
%%              "calls": 2,
%%              "cpt": 158.5,
%%              "occupancy": 26.505016722408026
%%            },
%%            {
%%              "coverage": "last_30m",
%%              "calls": 2,
%%              "cpt": 158.5,
%%              "occupancy": 26.505016722408026
%%            },
%%            {
%%              "coverage": "last_hr",
%%              "calls": 2,
%%              "cpt": 158.5,
%%              "occupancy": 26.505016722408026
%%            },
%%            {
%%              "coverage": "today",
%%              "calls": 2,
%%              "cpt": 158.5,
%%              "occupancy": 24.44101773323053
%%            },
%%            {
%%              "coverage": "this_week",
%%              "calls": 2,
%%              "cpt": 158.5
%%              "occupancy": 24.44101773323053
%%            },
%%            {
%%              "coverage": "this_month",
%%              "calls": 2,
%%              "cpt": 158.5,
%%              "occupancy": 24.44101773323053
%%            }
%%          ]
%%        }
%%      }
%%    ]
%%  }'''
%% @end
get_agents(_St) ->
	Agents = ouc_agents:get_online_agents(),
	RStats = ouc_rstat:get_agent_rstats(),
	AgentsArr = [{[
		{username, nob(Username)},
		{profile, nob(Profile)},
		{login_time, util:now_ms(LoginTime)},
		{first_name, nob(FirstName)},
		{last_name, nob(LastName)},
        {reach_node, ReachNode},
		{location, nob(Location)},
		{job_title, nob(JobTitle)},
		{skills, cpx_json_util:enc_skills(Skills)},
		{avatar, nob(Avatar)},
		{state, cpx_json_util:enc_agent_state(State)},
		{active_channels, enc_channels(Channels)},
		{rstats, enc_rstats(Username, RStats)}]} ||
		#online_agent{username=Username,
                      profile=Profile,
                      login_time=LoginTime,
			          first_name=FirstName,
                      last_name=LastName,
                      reach_node=ReachNode,
                      location=Location,
			          job_title=JobTitle,
                      skills=Skills,
                      state=State,
                      active_channels=Channels,
			          avatar=Avatar}    <- Agents],
	 {[
        {agents, AgentsArr}
     ]}.

%% @doc Subscribe to online agents changes.
%% === Arguments: ===
%% ```none (internal state) '''
%% === Answer: ===
%% ```Same as get_agents.
%% '''
%% @see ouc_sup_rpc:get_agents/1
%% @end
subscribe_agents(_St) ->
	ouc_agents:subscribe(),
	get_agents(_St).

-spec kick_agent(_St::term(), AgentLogin::string()) -> {[{kicked, true}]} | {atom(), integer(), binary()}.
%% @doc Kick an agent out.
%% === Arguments: ===
%% ```AgentLogin '''
%% === Answer: ===
%% ```{"kicked": "true"} OR
%%    {"error": "No agent"}
%% '''
%% @end
kick_agent(_St, AgentLogin) ->
  case agent_manager:query_agent(b2l(AgentLogin)) of
    {true, Pid} ->
      agent:stop(Pid, normal, kicked),
      {[{kicked, true}]};
    false ->
      err(no_agent)
  end.


%% @doc Get a list with details about the existing queues.
%% === Arguments: ===
%% ```none (internal state) '''
%% === Answer: ===
%% ```
%%  {
%%    "queues": [
%%      {
%%        "name": "default_queue",
%%        "call_in_queue": 1,
%%        "calls_connected": 1,
%%        "lines": [
%%          {
%%            "name": "Line90",
%%            "extension": 90,
%%            "did_number": null,
%%            "client": "client name"
%%          }
%%        ],
%%        "rstats": [
%%          {
%%            "coverage": "last_15m",
%%            "occupancy": 26.505016722408026,
%%            "cpt": 158.5,
%%            "calls": 2,
%%            "abandoned_calls": 0,
%%            "average_abandon_duration": "infinity",
%%            "average_wait_duration": 2.0,
%%            "max_wait_duration": 3
%%          },
%%          {
%%            "coverage": "last_30m",
%%            "occupancy": 26.505016722408026,
%%            "cpt": 158.5,
%%            "calls": 2,
%%            "abandoned_calls": 0,
%%            "average_abandon_duration": "infinity",
%%            "average_wait_duration": 2.0,
%%            "max_wait_duration": 3
%%          },
%%          {
%%            "coverage": "last_hr",
%%            "occupancy": 26.505016722408026,
%%            "cpt": 158.5,
%%            "calls": 2,
%%            "abandoned_calls": 0,
%%            "average_abandon_duration": "infinity",
%%            "average_wait_duration": 2.0,
%%            "max_wait_duration": 3
%%          },
%%          {
%%            "coverage": "today",
%%            "occupancy": 24.44101773323053,
%%            "cpt": 158.5,
%%            "calls": 2,
%%            "abandoned_calls": 0,
%%            "average_abandon_duration": "infinity",
%%            "average_wait_duration": 2.0,
%%            "max_wait_duration": 3
%%          },
%%          {
%%            "coverage": "this_week",
%%            "occupancy": 24.44101773323053,
%%            "cpt": 158.5,
%%            "calls": 2,
%%            "abandoned_calls": 0,
%%            "average_abandon_duration": "infinity",
%%            "average_wait_duration": 2.0,
%%            "max_wait_duration": 3
%%          },
%%          {
%%            "coverage": "this_month",
%%            "occupancy": 24.44101773323053,
%%            "cpt": 158.5,
%%            "calls": 2,
%%            "abandoned_calls": 0,
%%            "average_abandon_duration": "infinity",
%%            "average_wait_duration": 2.0,
%%            "max_wait_duration": 3
%%          }
%%        ]
%%      }
%%    ]
%%  }'''
%% @end
get_queues(_St) ->
	Queues = ouc_queues:get_queue_counts(),
	QRstats = ouc_rstat:get_queue_rstats(),

	QueuesArr = [{[
		{name, l2b(Name)},
		{calls_in_queue, CIQ},
		{calls_connected, CC},
		{lines, get_line_entries(Name)},
		{rstats, rstats_to_entries(QRstats, [{queue, Name}])}]} ||
		#queue_count{queue=Name, calls_queued=CIQ, calls_connected=CC} <- Queues],
	{[{queues, QueuesArr}]}.

%% @doc Subscribe to queues changes.
%% === Arguments: ===
%% ```none (internal state) '''
%% === Answer: ===
%% ```Same as get_queues.
%% '''
%% @see ouc_sup_rpc:get_queues/1
%% @end
subscribe_queues(_St) ->
	ouc_queues:subscribe(),
	get_queues(_St).


%% @doc Return a list of the current queued calls.
%% === Arguments: ===
%% ```none (internal state) '''
%% === Answer: ===
%% ```
%%  {
%%    "queued_calls": [
%%      {
%%        "id": "d4f03c6a-2f4c-11e4-a3a0-b9d403e83f7c",
%%        "time_queued": 1409296776598,
%%        "type": "voice",
%%        "line": "Line90",
%%        "queue": "default_queue",
%%        "reach_node": "ucdev.ezuce.ph",
%%        "skills": [
%%          "english",
%%          "german",
%%          {
%%            "client": "client name"
%%          }
%%        ],
%%        "callerId": "200",
%%        "client": "client name",
%%        "source_module": "freeswitch_media",
%%        "total_agent_count": 2,
%%        "available_agent_count": 1,
%%        "idle_agent_count": 1
%%      }
%%    ]
%%  }'''
%% @end
get_queued_calls(_St) ->
	Calls = ouc_calls:get_queued_calls(),
	{[
        {queued_calls, enc_calls(Calls)}
    ]}.

%% @doc Subscribe to get the list of queued calls.
%% === Arguments: ===
%% ```none (internal state) '''
%% === Answer: ===
%% ```Same as get_queued_calls.'''
%% @see ouc_sup_rpc:get_queued_calls/1
%% @end
subscribe_queued_calls(_St) ->
	ouc_calls:subscribe(),
	get_queued_calls(_St).


%% @doc Set the state of an agent.
%% === Arguments: ===
%% <ul>
%%  <li> `Agent' </li>
%%  <li> `State'</li>
%% </ul>
%% === Answer: ===
%% ```{"state": "available"} OR
%%    {"error": "Reason"}'''
%% @end
set_agent_state(_St, Agent, AState) ->
	case agent_manager:query_agent(b2l(Agent)) of
		{true, Pid} ->
			set_agent_pid_state(Pid, read_state(AState));
		_ ->
			err(no_agent)
	end.

%% @doc Monitor a call.
%% === Arguments: ===
%% <ul>
%%  <li> `AgentLogin' </li>
%%  <li> `ChannelId'</li>
%% </ul>
%% === Answer: ===
%% ```{"status": "initiated"} OR
%%    {"error": "Reason"}'''
%% @end
monitor_call(St, AgentLogin, ChanId) ->
	SuperPid = cpx_conn_state:get(St, agent_pid),
	join_call(monitor, SuperPid, b2l(AgentLogin), b2l(ChanId)).

%% @doc Barge a call.
%% === Arguments: ===
%% <ul>
%%  <li> `AgentLogin' </li>
%%  <li> `ChannelId'</li>
%% </ul>
%% === Answer: ===
%% ```{"status": "initiated"} OR
%%    {"error": "Reason"}'''
%% @end
barge_call(St, AgentLogin, ChanId) ->
	SuperPid = cpx_conn_state:get(St, agent_pid),
	join_call(barge, SuperPid, b2l(AgentLogin), b2l(ChanId)).


%% @doc Retrieve recordings for calls.
%% === Arguments: ===
%% <ul>
%%  <li> `FilterOptions - a json as:
%%  {"call_id": "", "queue": "", "line": "", "client": "", "caller_id": "", "agent": "", "skill": "", "start_timestamp": { "from": 1409298078317, "to": 1409298078318 } ' </li>
%%  <li> `SortOptions - json as:
%%  {"start_timestamp": 1, "client": 0, "queue": 0, "line": 0, "agent": 0, "recording_ms": 0 } '</li>
%%  <li> `Start - first entry index (number)'</li>
%%  <li> `End - last entry index (number)'</li>
%% </ul>
%% === Answer: ===
%% ```
%%  {
%%    "total_entries": 1,
%%    "recordings": [
%%      {
%%        "call_id": "d4f03c6a-2f4c-11e4-a3a0-b9d403e83f7c",
%%        "start_timestamp": 1409296776598,
%%        "caller_ani": "200",
%%        "caller_id": "200",
%%        "skills": [
%%          "english",
%%          "german",
%%        ],
%%        "client_id": "client_id",
%%        "client": "client name",
%%        "queue": "default_queue",
%%        "line": "line90",
%%        "agent": "1234",
%%        "recording_location": "/reach/portal/recordings?call_id=dce7c7b4-2f4f-11e4-a3d2-b9d403e83f7c&client=client_id&init_time=1409298078317",
%%        "file_exists": true,
%%        "recording_ms": 5232,
%%        "recording_file_type": "wav"
%%      }
%%    ]
%%  }'''
%% @end
get_recordings(_St, FilterOpts, SortOpts, Start, End) ->
	Db = ouc_db:get_db(reach),
	lager:debug("Recordings filter opts: ~p; sort opts: ~p", [FilterOpts, SortOpts]),
	Criteria = form_criteria(FilterOpts),
	Query = #search{criteria = Criteria,
		nskip = Start - 1,
		ndocs = End - (Start - 1)},
	QueryOpts = form_query_opts(SortOpts),
	lager:info("Recordings mongodb query: ~p; opts: ~p", [Criteria, QueryOpts]),
	{ok, DbRecordings} = Db:findOpt(?REC_META_COLL, Query, QueryOpts),

	Count = Db:count(?REC_META_COLL, Criteria),
	Recordings = enc_recordings(DbRecordings),
	{[{total_entries, Count}, {recordings, Recordings}]}.

% Internal functions
form_criteria({Opts}) when is_list(Opts) andalso Opts > 0 ->
	form_criteria(Opts, []);
form_criteria(_) ->
	[].

form_criteria([], Acc) ->
	Acc;
form_criteria([{_, null}|Opts], Acc) ->
	form_criteria(Opts, Acc);
form_criteria([{<<"call_id">>, V}|Opts], Acc)
		when is_binary(V) andalso byte_size(V) > 0 ->
	form_criteria(Opts, [eq(<<"call_id">>, V) | Acc]);
form_criteria([{<<"queue">>, V}|Opts], Acc)
		when is_binary(V) andalso byte_size(V) > 0 ->
	form_criteria(Opts, [eq(<<"queue">>, V) | Acc]);
form_criteria([{<<"line">>, V}|Opts], Acc)
		when is_binary(V) andalso byte_size(V) > 0 ->
	form_criteria(Opts, [eq(<<"line">>, V) | Acc]);
form_criteria([{<<"client">>, V}|Opts], Acc)
		when is_binary(V) andalso byte_size(V) > 0 ->
	form_criteria(Opts, [eq(<<"client">>, V) | Acc]);
form_criteria([{<<"caller_id">>, V}|Opts], Acc)
		when is_binary(V) andalso byte_size(V) > 0 ->
	form_criteria(Opts, [search_or([eq(<<"caller_ani">>, V), starts_with(<<"caller_id">>, V)]) | Acc]);
form_criteria([{<<"agent">>, V}|Opts], Acc)
		when is_binary(V) andalso byte_size(V) > 0 ->
	form_criteria(Opts, [eq(<<"agent">>, V) | Acc]);
form_criteria([{<<"skill">>, V}|Opts], Acc)
		when is_binary(V) andalso byte_size(V) > 0 ->
	form_criteria(Opts, [has_elem(<<"skills">>, V) | Acc]);
form_criteria([{<<"start_timestamp">>,
		{[{<<"from">>, FromMs},{<<"to">>, ToMs}]}}|Opts], Acc)
		when is_integer(FromMs) andalso is_integer(ToMs) ->
	From = util:ms_to_erl_now(FromMs),
	To = util:ms_to_erl_now(ToMs),
	form_criteria(Opts, [range(<<"start_timestamp">>, From, To) | Acc]);
form_criteria([_|Opts], Acc) ->
	form_criteria(Opts, Acc).

eq(Field, Value) ->
	{Field, Value}.
has_elem(Field, Value) ->
	eq(Field, Value).
starts_with(Field, Pattern) ->
	{Field, {regex, <<"^", Pattern/binary>>, []}}.
range(Field, Min, Max) ->
	{Field, {in, {gte, Min}, {lte, Max}}}.
search_or(Criteria) ->
	{'or', Criteria}.

form_query_opts({SortOpts}) when is_list(SortOpts) andalso length(SortOpts) > 0 ->
	[{sort, SortOpts}];
form_query_opts(_) ->
	[].

enc_recordings(Recordings) ->
	[enc_recording(RecProps) || RecProps <- Recordings].


%% ---------------------------------------------------------------------------------
-spec(enc_recording/1 :: (RecProps :: list()) -> list()).
%% @doc Given a proplist that contains a database entry corresponding to a
%%  recording, returns a proplist compliant with the JSON-rpc json response that
%%  should be returned to the interface.
%% @end
%% --------------------------------------------------------------------------------
enc_recording(RecProps) ->
  RecList = lists:map(fun({K, V}) ->
    case K of
      <<"_id">> -> {};
      <<"start_timestamp">> ->
        {start_timestamp, util:now_ms(V)};
      <<"recording_location">> ->
        Loc = case proplists:get_value(<<"db_uploaded">>, RecProps) of
          <<"true">> ->
            RootUri = ouc_handler_util:get_config(root_uri),
            iolist_to_binary([RootUri, V]);
          _ ->
            null
        end,
        {recording_location, Loc};
      <<"db_uploaded">> ->
        {file_exists, binary_to_atom(V, utf8)};
      _ ->
        {binary_to_atom(K, utf8), V}
    end
  end, RecProps),
  [ X || X <- RecList, X =/= {} ].


-spec enc_channels([#active_channel{}]) -> json().
enc_channels(Channels) ->
	lists:map(fun(Ch) ->
		Type = Ch#active_channel.type,
		State = Ch#active_channel.state,
		Client = Ch#active_channel.client,
		CallId = Ch#active_channel.callid,
		CallerId = case Ch#active_channel.callerid of
			{X, Y} ->
				iolist_to_binary([X, $\ , Y]);
			_ ->
				null
		end,
		ChanId = Ch#active_channel.channelid,
		StateChanges = Ch#active_channel.state_changes,
		{[
			{type, Type},
			{state, State},
			{client, nob(Client)},
			{callid, nob(CallId)},
			{callerid, CallerId},
			{channelid, nob(ChanId)},
			{state_changes, cpx_json_util:enc_state_changes(StateChanges)}
		]}
	end, Channels).

-spec enc_calls([#qcall{}]) -> json().
enc_calls(Calls) ->
	lists:map(fun(C) ->
		Id = C#qcall.id,
		InitTime = C#qcall.time_queued,
		Type = C#qcall.type,
		Line = C#qcall.line,
		Queue = C#qcall.queue,
		Skills = C#qcall.skills,
		{_CIDInternal, CIDDisplay} = C#qcall.callerid,
		Client = C#qcall.client,
		SourceModule = C#qcall.source_module,
		TotalAgts = C#qcall.total_agent_count,
		AvlAgts = C#qcall.available_agent_count,
		IdleAgts = C#qcall.idle_agent_count,
        ReachNode = C#qcall.reach_node,

		{[
			{id, nob(Id)},
			{time_queued, InitTime},
			{type, Type},
			{line, nob(Line)},
			{queue, nob(Queue)},
            {reach_node, ReachNode},
			{skills, cpx_json_util:enc_skills(Skills)},
			{callerid, nob(CIDDisplay)},
			{client, nob(Client)},
			{source_module, SourceModule},
			{total_agent_count, TotalAgts},
			{available_agent_count, AvlAgts},
			{idle_agent_count, IdleAgts}]}
	end, Calls).

enc_rstats(Login, RStats) ->
	Ndx = [{agent, Login}],
	%% @todo handle errors
	GetStat = fun(R, P) ->
		fmt_stat_val(ouc_rstat_snapshot:get_stat_val(RStats, R, P, Ndx))
	end,
	RNames = ouc_rstat_snapshot:get(RStats, rnames),

	try
		{[{coverages, [{[
			{coverage, R},
			{calls, GetStat(R, ?PROP_TE_CALL_COUNT)},
			{cpt, GetStat(R, ?PROP_AvE_CALL_DURATION)},
			{occupancy, GetStat(R, ?PROP_PcS_OCCUPANCY)}
		]} || R <- RNames]}]}
	catch
		_:Reason ->
			lager:warning("Unable to encode rstats for: ~p due to: ~p -- ~p",
				[Login, Reason, erlang:get_stacktrace()]),
			null
	end.

read_state(<<"available">>) ->
	available;
read_state(<<"released">>) ->
	released;
read_state(AState) ->
	case ej:get({"released"}, AState) of
		RelCode when is_binary(RelCode) ->
			case agent_auth:get_release(b2l(RelCode)) of
				{ok, Rel} ->
					{released, Rel};
				_ ->
					{error, invalid_rel}
			end;
		_ ->
			{error, invalid_rel}
	end.

counts_to_entries(#profile_counts{total = Total, released = Released, idle=Idle, ringing=Ringing, oncall=Oncall, wrapup=Wrapup}) ->
	{[
		{total, Total},
		{released, Released},
		{idle, Idle},
		{ringing, Ringing},
		{oncall, Oncall},
		{wrapup, Wrapup}
	]}.

rstats_to_entries(Snap, Ndx) ->
	RNames = ouc_rstat_snapshot:get(Snap, rnames),
	[begin
		Get = fun(P) ->
			fmt_stat_val(ouc_rstat_snapshot:get_stat_val(Snap, R, P, Ndx))
		end,
		{[
			{coverage, R},
			{occupancy, Get(?PROP_PcS_OCCUPANCY)},
			{cpt, Get(?PROP_AvE_CALL_DURATION)},
			{calls, Get(?PROP_TE_CALL_COUNT)},
			{abandoned_calls, Get(?PROP_TE_ABANDON_COUNT)},
			{average_abandon_duration, Get(?PROP_AvE_ABANDON_DURATION)},
			{average_wait_duration, Get(?PROP_AvE_WAIT_DURATION)},
			{max_wait_duration, Get(?PROP_ME_WAIT_DURATION)}
		]}
	end || R <- RNames].

% relavl_entry({R, A}) ->
% 	{[{released, R}, {available, A}]}.

set_agent_pid_state(APid, available) ->
	ok = agent:set_release(APid, none),
	{[{state, available}]};
set_agent_pid_state(APid, released) ->
	ok = agent:set_release(APid, default),
	{[{state, released}]};
set_agent_pid_state(APid, {released, R}) ->
	agent:set_release(APid, {R#release_opt.id, R#release_opt.label, R#release_opt.bias}),
	{[{state, released}, {release, relopt_entry(R)}]};
set_agent_pid_state(_APid, _) ->
	err(invalid_rel).

err(invalid_rel) ->
	%% from cpx_agent_rpc
	{error, 4001, <<"Invalid/Missing release code">>};
err(no_agent) ->
	{error, 6001, <<"No agent">>}.

relopt_entry(#release_opt{id=Id, label=Label, bias=B}) ->
	Bias = case B of
		N when N < 0 -> negative;
		N when N > 0 -> positive;
		_ -> neutral
	end,
	{[{id, l2b(Id)}, {name, nob(Label)}, {bias, Bias}]}.

-spec get_line_entries(Queue::string()) -> json().
get_line_entries(Queue) ->
	Db = ouc_db:get_db(imdb),
	{ok, Lines} = Db:findOpt(<<"entity">>, #search{criteria = [{<<"ent">>, <<"openacdline">>}, {<<"qnm">>, l2b(Queue)}]}, []),
	lists:sort(lists:map(fun(Props) ->
		Get = fun(Q) -> ej:get(Q, Props, null) end,
		[Ext|_] = re:split(Get({"ident"}), "@", [{return, binary}]),
		{[{name, Get({"linm"})},
			{extension, Ext},
			{did_number, Get({"did"})},
			{client, Get({"clnm"})}]}
	end, Lines)).

join_call(Type, SuperPid, AgentLogin, ChanId) ->
	case agent_state_manager:get_channel(AgentLogin, ChanId) of
		ChanPid when is_pid(ChanPid) ->
			{ok, Handler} = ouc_call_monitor:get_handler(SuperPid),
			case ouc_call_monitor:join_call(Handler, Type, AgentLogin, ChanId, ChanPid) of
				{ok, initiated} -> {[{status, initiated}]};
				{error, Reason} -> get_error(Reason)
			end;
		_ ->
			get_error(channel_not_found)
	end.

get_error(channel_not_found) ->
	{error, 4002, <<"Channel not found">>};
get_error(call_not_found) ->
	{error, <<"4002">>, <<"Call not found">>};
get_error(invalid_agent_state) ->
	{error, <<"4003">>, <<"Supervisor should be released to monitor/barge">>};
get_error(unable_to_initiate) ->
	{error, <<"4004">>, <<"Error in initiating call">>};
get_error(no_endpoint) ->
	{error, <<"4005">>, <<"Unable to get endpoint">>}.

fmt_stat_val(X) when is_number(X) -> X;
fmt_stat_val(infinity) -> infinity;
fmt_stat_val(_) -> null.

-ifdef(TEST).

t_st() ->
	cpx_conn_state:new(#agent{login="1000"}).

% t_expect_profiles(Profiles) ->
% 	meck:expect(agent_auth, get_profiles, 0,
% 			{ok, [#agent_profile{name=N, id=N, skills=Sk} || {N,Sk} <- Profiles]}).

% get_subscribe_agent_profiles_test_() ->
% 	{setup, fun() ->
% 		application:start(gproc),
% 		meck:new(agent_auth),
% 		meck:new(ouc_agent_profiles),

% 		meck:expect(ouc_agent_profiles, subscribe, 0, ok),

% 		t_expect_profiles([{"sales", ['english', {'_brand', "Client 1"}, '_all']},
% 			{"marketing", ['english', {'_brand', "Client 2"}, {'_queue', "marketing_queue"}]}])
% 	end, fun(_) ->
% 		meck:unload()
% 	end, [{"get", fun() ->
% 		meck:expect(ouc_agent_profiles, get_profiles, 0, [
% 			t_profile_stats("sales", {5, 7}, {8, 11}, {2, 1}, {4, 3}, last_30m, 0.5, 2.5, 3),
% 			t_profile_stats("marketing", {6, 8}, {9, 12}, {3, 2}, {5, 4}, last_30m, 0.7, 2.6, 9)
% 		]),

% 		?assertEqual({[{profiles, [
% 			{[{name, <<"sales">>}, {skills, [english, {[{client, <<"Client 1">>}]}, '_all']},
% 			  {counts, {[
% 			  	{idle, {[{released, 5}, {available, 7}]}},
% 			  	{ringing, {[{released, 8}, {available, 11}]}},
% 			  	{oncall, {[{released, 2}, {available, 1}]}},
% 			  	{wrapup, {[{released, 4}, {available, 3}]}}
% 			  ]}},
% 			  {rstats, [{[
% 				{coverage, last_30m},
% 				{occupancy, 0.5},
% 				{cpt, 2.5},
% 				{calls, 3}
% 			  ]}]}]},
% 			{[{name, <<"marketing">>}, {skills, [english, {[{client, <<"Client 2">>}]}, {[{queue, <<"marketing_queue">>}]}]},
%  			  {counts, {[
% 			  	{idle, {[{released, 6}, {available, 8}]}},
% 			  	{ringing, {[{released, 9}, {available, 12}]}},
% 			  	{oncall, {[{released, 3}, {available, 2}]}},
% 			  	{wrapup, {[{released, 5}, {available, 4}]}}
% 			  ]}},
% 			  {rstats, [{[
% 				{coverage, last_30m},
% 				{occupancy, 0.7},
% 				{cpt, 2.6},
% 				{calls, 9}
% 			  ]}]}]}
% 		]}]}, get_agent_profiles(t_st()))
% 	end}, {"subscribe", fun() ->
% 		meck:expect(ouc_agent_profiles, get_profiles, 0,
% 			[{"sales", 10, 2}, {"marketing", 3, 7}]),

% 		St = t_st(),
% 		?assertEqual(get_agent_profiles(St),
% 			subscribe_agent_profiles(St)),

% 		?assert(meck:called(ouc_agent_profiles, subscribe, [], self()))
% 	end}]}.

get_agents_test_() ->
	{setup, fun() ->
		meck:new(ouc_agents),

		meck:new(ouc_rstat),
		meck:expect(ouc_rstat, get_agent_rstats, 0, t_snapshot())
	end, fun(_) ->
		meck:unload()
	end, [{"get agents", fun() ->

		meck:expect(ouc_agents, get_online_agents, 0, [
			#online_agent{
				username="agent1",
				profile="sales",
				login_time={13,14,14000},
				skills=['english', {'_brand', "Client 1"}, '_all'],
				state={released, {"rel0", "Lunch", 0}},
				active_channels=[
					#active_channel{
						type=voice,
						state=ringing,
						client="Client 1",
						callid="uuid",
						callerid={"Caller1","Caller1"}}],
						first_name="first1",
						last_name="last1",
						location="loc1",
						job_title="job1",
						avatar="http://ezuce.com/avatar/agent1"}
		]),

		?assertEqual({[{agents, [
			{[
				{username, <<"agent1">>},
				{profile, <<"sales">>},
				{login_time, 13000014014},
				{first_name, <<"first1">>},
				{last_name, <<"last1">>},
				{location, <<"loc1">>},
				{job_title, <<"job1">>},
				{skills, [english, {[{client, <<"Client 1">>}]}, '_all']},
				{avatar, <<"http://ezuce.com/avatar/agent1">>},
			  	{state, {[
			  		{released,
			  			{[{id, <<"rel0">>},
			  				{name, <<"Lunch">>}, {bias, neutral}]}}]}},
			  	{active_channels,
			  		[{[{type,voice}, {state,ringing}, {client,<<"Client 1">>},
			  			{callid,<<"uuid">>}, {callerid,<<"Caller1 Caller1">>}]}]},
			  	{rstats,
					{[{coverages, [{[
						{coverage,last_30m},
						{calls,null},
						{cpt,null},
						{occupancy,null}]}
					]}]}}
			]}
		]}]}, get_agents(t_st()))
	end}]}.

t_samp_lines() ->
	[{<<"sales">>, [[{<<"_id">>,<<"OpenAcdLine6">>},
		{<<"ident">>,<<"6@oacddev.ezuce.com">>},
		{<<"ent">>,<<"openacdline">>},
		{<<"linm">>,<<"Line6">>},
		{<<"did">>,<<"7000006">>},
		{<<"clnm">>,<<"Client 1">>}]]},
	{<<"marketing">>, []}].

get_queues_test_() ->
	{spawn, {setup, fun() ->
		application:start(gproc),
		meck:new(mongoapi),

		meck:expect(mongoapi, new, fun(N, Db) -> {mongoapi, N, Db} end),
		meck:expect(mongoapi, findOpt,
			fun(_, #search{criteria = Criteria}, _, _) ->
				Q = proplists:get_value(<<"qnm">>, Criteria),
				{ok, proplists:get_value(Q, t_samp_lines())}
			end),

		meck:new(ouc_rstat),
		meck:expect(ouc_rstat, get_queue_rstats, 0, t_snapshot()),

		call_queue_config:start(),
		call_queue_config_ets:load_queues(["sales"]),
		ouc_queues:start()
	end, fun(_) ->
		ouc_queues:stop(),
		meck:unload()
	end, [{"get queues", fun() ->
		?assertEqual({[{queues, [{[
			{name, <<"sales">>},
			{calls_in_queue, 0},
			{calls_connected, 0},
			{lines, [{[
				{name, <<"Line6">>},
				{extension, <<"6">>},
				{did_number, <<"7000006">>},
				{client, <<"Client 1">>}]}]},
			{rstats,[{[
				{coverage,last_30m},
				{occupancy,null},
				{cpt,null},
				{calls,null},
				{abandoned_calls,null},
				{average_abandon_duration,null},
				{average_wait_duration,null},
				{max_wait_duration,null}
			]}]}]}]}]},
       	get_queues(t_st()))
	end}]}}.

get_calls_test_() ->
	{setup, fun() ->
		application:start(gproc),
		SampLine = [{<<"_id">>,<<"OpenAcdLine6">>},
			{<<"ident">>,<<"6@oacddev.ezuce.com">>},
			{<<"ent">>,<<"openacdline">>},
			{<<"linm">>,<<"Line6">>},
			{<<"qnm">>, <<"sales">>}],
		meck:new(mongoapi),
		meck:expect(mongoapi, new, fun(N, Db) -> {mongoapi, N, Db} end),
		meck:expect(mongoapi, findOne, 3, {ok, SampLine}),

		{Caller, Queue, Line, Client, InqueueTs, Skills} = {"6001", "sales", "6", "client1", {13,10,10}, ['english', {'_brand', "Client 1"}]},
		CallRec = #call{id="id"++Caller, source=Caller, type=voice, callerid={Caller, Caller}, queue=Queue, dnis=Line, skills=Skills, client=#client{label=Client}},
		MediaProp = #cpx_gen_media_prop{call=CallRec, state=inqueue, state_changes=[{inqueue, InqueueTs}, {inivr, {13,12,9}}, {init,{13,10,8}}]},
		gproc:add_global_property(cpx_media, MediaProp),
		ouc_calls:start()
	end, fun(_) ->
		ouc_calls:stop()
	end, [{"get queues", fun() ->
		?assertEqual({[{queued_calls, [
			{[{id, <<"id6001">>}, {time_queued, 13000010000}, {type, voice}, {line, <<"Line6">>}, {queue, <<"sales">>},
			{skills, [english, {[{client, <<"Client 1">>}]}]}, {callerid, <<"6001">>}, {client, <<"client1">>}, {total_agent_count, 0}, {available_agent_count, 0}, {idle_agent_count, 0}]}]}]},
		get_queued_calls(t_st()))
	end}]}.

set_agent_state_test_() ->
	Pid = spawn(fun() -> ok end),

	{setup, fun() ->
		meck:new(agent_manager),
		meck:expect(agent_manager, query_agent,
			fun("agent1") -> {true, Pid};
				(_) -> false end),

		meck:new(agent_auth),
		meck:expect(agent_auth, get_release, fun("relopt1") -> {ok,
			#release_opt{id="relopt1", label="Release 1", bias=-1}};
		(_) -> none
		end),

		meck:new(agent),
		meck:expect(agent, set_release, 2, ok)
	end, fun(_) ->
		meck:unload()
	end, [{"non existing agent", fun() ->
		?assertEqual(err(no_agent),
			set_agent_state(t_st(), <<"noagent">>, <<"available">>))
	end}, {"invalid release code", fun() ->
		?assertEqual(err(invalid_rel),
			set_agent_state(t_st(), <<"agent1">>, {[{<<"released">>, <<"relopt99">>}]}))
	end}, {"go available", fun() ->
		?assertEqual({[{state, available}]},
			set_agent_state(t_st(), <<"agent1">>, <<"available">>)),
		?assert(meck:called(agent, set_release, [Pid, none], self()))
	end}, {"go released - proper code", fun() ->
		?assertEqual({[{state, released},
			{release, {[{id, <<"relopt1">>}, {name, <<"Release 1">>}, {bias, negative}]}}]},
			set_agent_state(t_st(), <<"agent1">>, {[{<<"released">>, <<"relopt1">>}]})),
		?assert(meck:called(agent, set_release, [Pid, {"relopt1", "Release 1", -1}], self()))
	end}]}.

t_snapshot() ->
	{ok, Conf} = ouc_rstat_conf:new([{rules, [
		{dynamic, last_30m, {minute, 30}, 5}
	]}]),

	{ok, RStore} = ouc_rstat_store:new(ouc_rstat_store_dict, Conf, []),
	{ok, Snap} = ouc_rstat_snapshot:new(RStore, 10000, []),
	Snap.

-endif.
