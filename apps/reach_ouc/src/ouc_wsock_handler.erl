-module(ouc_wsock_handler).
-author("jvliwanag").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("oacd_ouc.hrl").
-include("ouc_rstat.hrl").
-include_lib("reach_core/include/agent.hrl").

-export([handle_wsinfo/2]).

-import(cpx_json_util, [l2b/1, nob/1]).

-type state() :: cpx_conn_state:state().

-spec handle_wsinfo(state(), any()) -> {error, unhandled} | {ok, Json::any(), state()}.
handle_wsinfo(Conn, {ouc_agent_profile_count, Diffs}) ->
	Resp = {[{event, profile_count},
		{data, {[profile_diff_to_entry(X) || X <- Diffs]}}
	]},
	{ok, Resp, Conn};
handle_wsinfo(Conn, {ouc_queue_count, Diffs}) ->
	Resp = {[{event, queue_count},
		{data, {[queue_diff_to_entry(X) || X <- Diffs]}}
	]},
	{ok, Resp, Conn};
handle_wsinfo(Conn, {ouc_queued_calls, Calls}) ->
    %PrettyPrint = fun (X) -> 
    %    lager:debug("Incoming diff: ~p",[lager:pr(X, ?MODULE)])
    %end,
    %lists:map(PrettyPrint, Calls),
	Resp = {[{event, queued_calls_update},
		{data, {[qcall_data_to_entry(X) || X <- Calls]}
	}]},
	{ok, Resp, Conn};
handle_wsinfo(Conn, {ouc_agents, Agents}) ->
	Resp = {[{event, agents_update},
		{data, {[agent_data_to_entry(X) || X <- Agents]}
	}]},
	{ok, Resp, Conn};
handle_wsinfo(Conn, {ouc_set_agent_profiles_subscribed, T}) ->
	Conn1 = cpx_conn_state:set_prop(Conn, ouc_agent_profiles_subscribed, T),
	{ok, undefined, Conn1};
handle_wsinfo(Conn, {ouc_rstat_update, _I}) ->
	Self = self(),

	IsAgentRstatsSubscribed = ouc_agents:is_subscribed(),

	spawn(fun() ->
		maybe_send_profile_rstat_updates(Conn, Self),
		send_my_rstat_updates(Conn, Self),
		case IsAgentRstatsSubscribed of
			true ->
				send(get_agent_rstat_updates(), Self);
			_ ->
				ok
		end
	end),
	{ok, undefined, Conn};
handle_wsinfo(Conn, {ouc_ws_send, Resp}) ->
	{ok, Resp, Conn};
handle_wsinfo(Conn, {ouc_call_monitor_result, {Agent, ChanId, {Status, Data}}}) ->
	Resp = {[{event, call_monitor_result},
		{agent, Agent},
		{channelid, ChanId},
		{status, Status},
		{data, {Data}}
	]},
	{ok, Resp, Conn};
handle_wsinfo(Conn, {ouc_call_monitor_ended, {Agent, ChanId, Data}}) ->
	Resp = {[{event, call_monitor_ended},
		{agent, Agent},
		{channelid, ChanId},
		{data, {Data}}
	]},
	{ok, Resp, Conn};
handle_wsinfo(Conn, {ouc_call_barge_result, {Agent, ChanId, {Status, Data}}}) ->
	Resp = {[{event, call_barge_result},
		{agent, Agent},
		{channelid, ChanId},
		{status, Status},
		{data, {Data}}
	]},
	{ok, Resp, Conn};
handle_wsinfo(Conn, {ouc_call_barge_ended, {Agent, ChanId, Data}}) ->
	Resp = {[{event, call_barge_ended},
		{agent, Agent},
		{channelid, ChanId},
		{data, {Data}}
	]},
	{ok, Resp, Conn};
handle_wsinfo(Conn, {agent, {forced_release, ring_init_failed}}) ->
	Resp = {[{event, forced_release},
		{data, {[{code, <<"4001">>}, {message, <<"Ring init on endpoint failed">>}]}
	}]},
	{ok, Resp, Conn};
handle_wsinfo(Conn, {outbound_call, {CallId, Data}}) ->
	Resp = {[{event, outbound_call_update},
		{callid, CallId},
		{data, {Data}}
	]},
	{ok, Resp, Conn};
handle_wsinfo(Conn, {ouc_transfer_agents_update, {A, Stat}}) ->
	case cpx_conn_state:get(Conn, agent_login) =/= proplists:get_value(login, A) of
		true ->
			Resp = {[{event, transfer_agents_update},
				{data, [form_agent({A, Stat})]}
			]},
			{ok, Resp, Conn};
		_ ->
			{ok, undefined, Conn}
	end;
handle_wsinfo(Conn, {ouc_my_rolling_stats_result, MyStats}) ->
	Resp = {[
		{event, my_rolling_stats_update},
		{data, MyStats}
	]},
	{ok, Resp, Conn};
handle_wsinfo(_Conn, Msg) ->
    lager:debug("Received message will be handled by a fallback module: ~p", [Msg]),
	{error, unhandled}.

%% Internal

send(R, Pid) ->
	Pid ! {ouc_ws_send, R}.

maybe_send_profile_rstat_updates(Conn, Pid) ->
	case cpx_conn_state:get_prop(Conn, ouc_agent_profiles_subscribed)  of
		{ok, true} ->
			send(get_profile_rstat_updates(), Pid),
			send(get_queue_rstat_updates(), Pid);
		_ ->
			ok
	end.

send_my_rstat_updates(Conn, Pid) ->
	Login = cpx_conn_state:get(Conn, agent_login),
	Clients = cpx_conn_state:get(Conn, clients),
	RStats = ouc_rstat:track_my_client_rstats(Login, Clients, Pid),
	send({[{event, my_rolling_stats_update},
		{data, ouc_rpc_util:my_rstats_to_json(RStats, Login, Clients) }]}, Pid).

get_profile_rstat_updates() ->
	RStats = ouc_rstat:get_profile_rstats(),
	{[{event, profile_rstat_update},
		{data, {[
		{updates,
			get_rstat_update_entries(profile, RStats)}]}}]}.

get_queue_rstat_updates() ->
	RStats = ouc_rstat:get_queue_rstats(),
	{[{event, queue_rstat_update},
		{data, {[
		{updates,
			get_rstat_update_entries(queue, RStats)}]}}]}.

get_agent_rstat_updates() ->
	RStats = ouc_rstat:get_agent_rstats(),
	{[{event, agent_rstat_update},
		{data, {[
		{updates,
			get_rstat_update_entries(username, agent, RStats)}]}}]}.

profile_diff_to_entry({profile_update, Profile, Counts}) ->
	{list_to_binary(Profile), {[
		{total, Counts#profile_counts.total},
		{released, Counts#profile_counts.released},
		{idle, Counts#profile_counts.idle},
		{ringing, Counts#profile_counts.ringing},
		{oncall, Counts#profile_counts.oncall},
		{wrapup, Counts#profile_counts.wrapup}
	]}};
profile_diff_to_entry({profile_delete, Profile}) ->
	{list_to_binary(Profile), deleted}.

get_rstat_update_entries(Label, Snap) ->
	get_rstat_update_entries(Label, Label, Snap).
get_rstat_update_entries(Label, Key, Snap) ->
	Names = [N || [{T, N}] <- ouc_rstat_snapshot:get(Snap, indices),
		T =:= Key],

	RNames = ouc_rstat_snapshot:get(Snap, rnames),

	[rstat_update_to_entry(Label, Key, Name, R, Snap) ||
		Name <- Names, R <- RNames].

rstat_update_to_entry(Label, Key, Name, R, Snap) ->
	Ndx = [{Key, Name}],
	Get = fun(P) ->
		fmt_stat_val(ouc_rstat_snapshot:get_stat_val(Snap, R, P, Ndx))
	end,
	{[
		{Label, l2b(Name)},
		{coverage, R},
		{occupancy, Get(?PROP_PcS_OCCUPANCY)},
		{cpt, Get(?PROP_AvE_CALL_DURATION)},
		{calls, Get(?PROP_TE_CALL_COUNT)},
		{abandoned_calls, Get(?PROP_TE_ABANDON_COUNT)},
		{average_abandon_duration, Get(?PROP_AvE_ABANDON_DURATION)},
		{average_wait_duration, Get(?PROP_AvE_WAIT_DURATION)},
		{max_wait_duration, Get(?PROP_ME_WAIT_DURATION)}
	]}.

%% --------------------------------------------------------------------------------------
-spec agent_data_to_entry({string(), offline | #online_agent{}}) -> json().
%% @private
%% @doc Parses agent_data and transforms it to a json ready form to be delivered
%%      to the frontend.
%% @end
%% --------------------------------------------------------------------------------------
agent_data_to_entry({Username, offline}) ->
	{Username, offline};
agent_data_to_entry({Username, Agent}) when is_record(Agent, online_agent) ->
	#online_agent{profile=Profile,
                  login_time=LoginTime,
                  first_name=FirstName,
                  last_name=LastName,
                  location=Location,
                  job_title=JobTitle,
                  skills=Skills,
                  state=State,
                  avatar=Avatar,
                  reach_node=ReachNode,
                  active_channels=Channels} = Agent,
    {nob(Username), {[
        {profile, nob(Profile)},
        {login_time, util:now_ms(LoginTime)},
        {first_name, nob(FirstName)},
        {last_name, nob(LastName)},
        {location, nob(Location)},
        {job_title, nob(JobTitle)},
        {reach_node, ReachNode},
        {skills, cpx_json_util:enc_skills(Skills)},
        {avatar, nob(Avatar)},
        {state, cpx_json_util:enc_agent_state(State)},
        {active_channels, enc_channels(Channels)}
    ]}}.

-spec enc_channels([#active_channel{}]) -> json().
enc_channels(Channels) ->
	lists:map(fun(Ch) ->
		Type = Ch#active_channel.type,
		State = Ch#active_channel.state,
		Client = Ch#active_channel.client,
		CallId = Ch#active_channel.callid,
		CallerId = Ch#active_channel.callerid,
		ChanId = Ch#active_channel.channelid,
		StateChanges = Ch#active_channel.state_changes,
		{[{type, Type},
			{state, State}, {client, nob(Client)}, {callid, nob(CallId)},
			{callerid, caller_id_to_bin(CallerId)}, {channelid, nob(ChanId)},
			{state_changes, cpx_json_util:enc_state_changes(StateChanges)}]}
	end, Channels).

queue_diff_to_entry(Counts) when is_record(Counts, queue_count) ->
	{l2b(Counts#queue_count.queue), {[
		{calls_in_queue, Counts#queue_count.calls_queued},
		{calls_connected, Counts#queue_count.calls_connected}
	]}};
queue_diff_to_entry({queue_delete, Queue}) ->
	{l2b(Queue), deleted}.

%% ------------------------------------------------------------------------------------------------
-spec qcall_data_to_entry({Id::string(), offline | #qcall{}}) -> json().
%% @private
%% @doc Parses a qcall record and presents a json ready object ready to be served
%%      to the web frontend.
%% @end
%% ------------------------------------------------------------------------------------------------
qcall_data_to_entry({call_delete, CallId}) ->
	{l2b(CallId), deleted};

qcall_data_to_entry(Call) when is_record(Call, qcall) ->
	#qcall{id=Id,
           time_queued=TimeQueued,
           type=Type,
           line=Line,
           queue=Queue,
           reach_node=ReachNode,
           skills=Skills,
           callerid=CallerId,
           client=Client,
           source_module=SourceModule,
           total_agent_count=AgMatch,
           available_agent_count=AvAgMatch,
           idle_agent_count=IdleAgMatch} = Call,
    {l2b(Id), {[
        {time_queued, TimeQueued},
        {type, Type},
        {line, nob(Line)},
        {queue, nob(Queue)},
        {reach_node, ReachNode},
        {skills, cpx_json_util:enc_skills(Skills)},
        {callerid, caller_id_to_bin(CallerId)},
        {client, nob(Client)},
        {source_module, SourceModule},
        {total_agent_count, AgMatch},
        {available_agent_count, AvAgMatch},
        {idle_agent_count, IdleAgMatch}]}};

qcall_data_to_entry(Call) ->
    lager:error("QCall - is_record ~p ",[is_record(Call,qcall)]),
    lager:error("QCall - unindentified ~p",[Call]).


fmt_stat_val(X) when is_number(X) -> X;
fmt_stat_val(infinity) -> infinity;
fmt_stat_val(_) -> null.

caller_id_to_bin({X, Y}) when is_list(X), is_list(Y) ->
	iolist_to_binary([X, $\ , Y]);
caller_id_to_bin(_) ->
	null.

form_agent({A, offline}) ->
	{l2b(proplists:get_value(login, A)), offline};
form_agent({A, online}) ->
	{l2b(proplists:get_value(login, A)), {[
		{profile, l2b(proplists:get_value(profile, A))},
		{first_name, nob(proplists:get_value(first_name, A))},
		{last_name, nob(proplists:get_value(last_name, A))},
		{state, proplists:get_value(state, A)}]}}.

-ifdef(TEST).
%% @todo update unit tests
t_st() ->
	cpx_conn_state:new(#agent{login="agent"}).

handle_profile_count_test() ->
	St = t_st(),
	_K = handle_wsinfo(St, {ouc_agent_profile_count, [
			{profile_update, "sales", t_profile_counts(5, 2, 1, 1, 1, 0)},
			{profile_delete, "marketing"}
		]}),

	?assertEqual(
		{ok, {[{event, profile_count},
			{data, {[
				{<<"sales">>, {[
					{total, 5},
					{released, 2},
					{idle, 1},
					{ringing, 1},
					{oncall, 1},
					{wrapup, 0}
				]}},
				{<<"marketing">>, deleted}
			]}}
		]}, St},
		handle_wsinfo(St, {ouc_agent_profile_count, [
			{profile_update, "sales", t_profile_counts(5, 2, 1, 1, 1, 0)},
			{profile_delete, "marketing"}
		]})).

handle_queue_count_test() ->
	St = t_st(),
	?assertEqual(
		{ok, {[{event, queue_count},
			{data, {[
				{<<"sales">>, {[
					{calls_in_queue, 3},
					{calls_connected, 0}]}},
				{<<"marketing">>, deleted}
		]}}]}, St},
		handle_wsinfo(St, {ouc_queue_count,
			[#queue_count{queue="sales", calls_queued=3},
			{queue_delete, "marketing"}]})).

handle_qcall_update_test() ->
	St = t_st(),
	?assertEqual(
		{ok, {[{event, queued_calls_update},
			{data, {[
				{<<"id1">>, {[
					{time_queued, 13000010000}, {type, voice}, {line, <<"Line1">>}, {queue, <<"default_queue">>}, {skills, ['english', {[{client, <<"Client 1">>}]}]},
					{callerid, <<"6001 6001">>}, {client, <<"client1">>}, {total_agent_count, 2}, {available_agent_count, 1}, {idle_agent_count, 1}]}},
				{<<"id2">>, deleted}
		]}}]}, St},
		handle_wsinfo(St, {ouc_queued_calls,
			[#qcall{id = "id1", time_queued = 13000010000, dnis = "1", line = "Line1", queue = "default_queue",
			skills=['english', {'_brand', "Client 1"}], callerid = {"6001", "6001"}, client = "client1", type = voice, total_agent_count = 2, available_agent_count = 1, idle_agent_count = 1},
			{call_delete, "id2"}]})).

handle_agent_update_test() ->
	St = t_st(),
	?assertEqual(
		{ok, {[{event, agents_update},
			{data, {[{<<"agent1">>, {[{profile, <<"sales">>},
				{login_time, 13000014014}, {first_name, <<"first1">>}, {last_name, <<"last1">>},
				{location, <<"loc1">>}, {job_title, <<"job1">>},
				{skills, [english, {[{client, <<"Client 1">>}]}, '_all']}, {avatar, <<"https://oacddev.ezuce.com/sipxconfig/rest/avatar/agent1">>},
				{state, {[{released, {[{id, <<"rel0">>}, {name, <<"Lunch">>}, {bias, neutral}]}}]}},
				{active_channels, [{[{type,voice}, {state,ringing}, {client,<<"Client 1">>}, {callid, <<"uuid">>}, {callerid,<<"Caller1 Caller1">>}]}]}]}}]}
		}]}, St},
		handle_wsinfo(St, {ouc_agents, [{"agent1", #online_agent{username="agent1", profile="sales", login_time={13,14,14000},
					skills=['english', {'_brand', "Client 1"}, '_all'], avatar="https://oacddev.ezuce.com/sipxconfig/rest/avatar/agent1",
					state={released, {"rel0", "Lunch", 0}}, first_name="first1", last_name="last1", location="loc1", job_title="job1",
					active_channels=[#active_channel{type=voice, state=ringing, client="Client 1", callid="uuid", callerid={"Caller1","Caller1"}}]}}]})).

handle_agent_deleted_test() ->
	St = t_st(),
	?assertEqual(
		{ok, {[{event, agents_update},
			{data, {[{"agent1", offline}]}
		}]}, St},
		handle_wsinfo(St, {ouc_agents, [{"agent1", offline}]})).

handle_forced_release_test() ->
	St = t_st(),
	?assertEqual(
		{ok, {[{event, forced_release},
			{data, {[{code, <<"4001">>}, {message, <<"Ring init on endpoint failed">>}]}}
		]}, St},
		handle_wsinfo(St, {agent, {forced_release, ring_init_failed}})).

handle_unhandled_wsinfo_test() ->
	?assertEqual({error, unhandled},
		handle_wsinfo(t_st(), unknown_msg)).

t_profile_counts(Total, Released, Idle, Ringing, OnCall, WrapUp) ->
	#profile_counts{
		total=Total,
		released=Released,
		idle=Idle,
		ringing=Ringing,
		oncall=OnCall,
		wrapup=WrapUp
	}.

-endif.
