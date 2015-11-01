%% @doc Reach_ouc RPC module.
%% Exposes agent API calls over websockets - agent level only.
%% In order to use these calls one needs to do the following:
%% === Example ===
%% ```
%% $.oucUtils.loadGlobals()
%% this.callApi('ouc.get_contact_info')
%% '''
%% @end
-module(ouc_rpc).
-export([
	get_contact_info/1,
	set_contact_info/2,
	get_client/2,
	get_clients/1,
	get_lines/1,
	get_live_stats/1,
	get_my_rolling_stats/1,
	get_permission_profile/1,
	set_tab_layout/4,
	initiate_outbound/2,
	call_outbound/2,
	hold_outbound/1,
	unhold_outbound/1,
	hangup_outbound/1,
	transfer_outbound/2,
	conference_outbound/2,
	call_voicemail_outbound/3,
	get_transfer_agents/1,
	subscribe_transfer_agents/1,
	unsubscribe_transfer_agents/1,
	get_matched_agent_count/3,
	get_wsock_urls/1
]).
-import(cpx_json_util, [l2b/1, b2l/1, nob/1]).

-include("oacd_ouc.hrl").

-include("ouc_rstat.hrl").
-include_lib("reach_core/include/call.hrl").
-include_lib("erlmongo/include/erlmongo.hrl").
-include_lib("reach_core/include/gen_media.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("reach_core/include/agent.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(PINTOKEN_KEY, {"pntk"}).
-define(INQUEUE_STATES, [inqueue, inqueue_ringing]).
% -record(userLayout, {agent, tabs}).

-type state() :: cpx_conn_state:state().

%% @doc Get contact info for the current agent.
%% === Arguments: ===
%% ```none (internal state)'''
%% === Answer exampe: ===
%% ```
%% {
%%   "username": "1234",
%%   "first_name": "Bob",
%%   "last_name": "Builder",
%%   "email": "bob@builders.com",
%%   "home_address": {
%%     "street": "55 Builders Ave",
%%     "city", "Build City",
%%     "state": "New Bob",
%%     "country": "Bob",
%%     "zip": "5555"
%%   },
%%   "phone": {
%%     "internal": "1234",
%%     "cell": "+012314",
%%     "direct": "+012231"
%%   },
%%   "position": "Master Builder",
%%   "avatar": "http://build.com/bob",
%%   "manager": "Engineer"
%% }
%% '''
%% @end
-spec get_contact_info(state()) -> any().
get_contact_info(St) ->
	User = get_conn_info(St, agent_login),
	Db = ouc_db:get_db(profiles),
	{ok, Props} = Db:findOne(<<"userProfile">>, [{<<"m_userName">>, User}]),

	Get = fun(Q) -> ej:get(Q, Props, null) end,

	{[{username, Get({"m_userName"})},
		{first_name, Get({"m_firstName"})},
		{last_name, Get({"m_lastName"})},
		{email, Get({"m_emailAddress"})},
		{home_address, {[
			{street, Get({"m_homeAddress", "m_street"})},
			{city, Get({"m_homeAddress", "m_city"})},
			{state, Get({"m_homeAddress", "m_state"})},
			{country, Get({"m_homeAddress", "m_country"})},
			{zip, Get({"m_homeAddress", "m_zip"})}
			]}},
		{phone, {[
			{internal, Get({"m_userName"})},
			{cell, Get({"m_cellPhoneNumber"})},
			{direct, Get({"m_didNumber"})}
		]}},
		{position, Get({"m_jobTitle"})},
		{avatar, Get({"m_avatar"})},
		{manager, Get({"m_manager"})}]}.

%% @doc Sets contact info.
%% === Arguments: ===
%% ```ContactInfo (samme format as get_contact_info output)'''
%% === Answer: ===
%% `"success" 'or
%% ```{"error": "error in set profile"}'''
%% @see ouc_rpc:get_contact_info/1
%% @end
set_contact_info(St, ContactInfo) ->
	User = get_conn_info(St, agent_login),
	lager:info("Setting contact info for ~p to ~p", [User, ContactInfo]),
	Password = get_user_password(User),

	case get_contact_info_struct(User, Password) of
		{ok, GetStruct} ->
			NewStruct = build_contact_info_struct(json_build_map(), GetStruct, ContactInfo),
			lager:debug("Old contact info json: ~p", [GetStruct]),
			lager:debug("New contact info json: ~p", [NewStruct]),
			NewJson = encode_json(NewStruct),
			case set_contact_info_struct(NewJson, User, Password) of
				{ok, success} ->
					{ok, success};
				_ ->
					{error, 2, <<"error in set profile">>}
			end;
		_ ->
			{error, 2, <<"error in set profile">>}
	end.

%% @doc Gets client information.
%% === Arguments: ===
%% ``` ClientName '''
%% === Answer: ===
%% ```
%% {
%%   "name": "client_name",
%%   "avatar": "http://mega.com/avatar",
%%   "urlpop": "http://urlpop/",
%%   "call_dispositions": ["Send a Flyer", "Forward"]
%% }'''
%% @end
get_client(_St, Client) ->
	lager:debug("Getting client info for ~p", [Client]),
	Db = ouc_db:get_db(imdb),
	{ok, Props} = Db:findOne(<<"entity">>,
		[{<<"type">>, <<"openacdclient">>}, {<<"name">>, Client}]),
	Get = fun(Q) -> ej:get(Q, Props, null) end,
	Avt = case Get({"useExtAvt"}) of
			true ->
				Get({"extAvt"});
			_ ->
				Get({"avt"})
	end,

	{[{name, Get({"name"})},
		{avatar, Avt},
		{urlpop, Get({"urlpop"})},
		{call_dispositions, Get({"call_dispositions"})}]}.

%% @doc Get all clients information.
%% === Arguments: ===
%% ``` none (internal state)'''
%% === Answer: ===
%% ```
%% [
%%   {
%%     "name": "client_name",
%%     "avatar": "http://mega.com/avatar",
%%     "urlpop": "http://urlpop/",
%%     "call_dispositions": ["Send a Flyer", "Forward"]
%%   },
%%   ...
%% ]'''
%% @see ouc_rpc:get_client/2
%% @end
get_clients(_St) ->
	Db = ouc_db:get_db(imdb),
	{ok, Clients} = Db:findOpt(<<"entity">>, #search{criteria =
		[{<<"type">>, <<"openacdclient">>}]}, []),
	lists:map(fun(Props) ->
		Get = fun(Q) -> ej:get(Q, Props, null) end,
		Avt = case Get({"useExtAvt"}) of
			true ->
				Get({"extAvt"});
			_ ->
				Get({"avt"})
		end,

		{[{name, Get({"name"})},
			{avatar, Avt},
			{urlpop, Get({"urlpop"})},
			{call_dispositions, Get({"call_dispositions"})}]}
	end, Clients).

%% @doc Get information about the configured lines.
%% === Arguments: ===
%% ``` none (internal state)'''
%% === Answer: ===
%% ```{
%% "lines": [{
%%    "name": "Line90",
%%    "extension": "90",
%%    "did_number": "90",
%%    "client": "client name"
%%   }]
%% }'''
%% @end
get_lines(_St) ->
	Db = ouc_db:get_db(imdb),
	{ok, Lines} = Db:findOpt(<<"entity">>, #search{criteria = [{<<"ent">>, <<"openacdline">>}]}, []),
	Entries = lists:map(fun(Props) ->
		Get = fun(Q) -> ej:get(Q, Props, null) end,
		[Ext|_] = re:split(Get({"ident"}), "@", [{return, binary}]),
		{[{name, Get({"linm"})},
			{extension, Ext},
			{did_number, Get({"did"})},
			{client, Get({"clnm"})}]}
	end, Lines),
	{[{lines, Entries}]}.

get_permission_profile(_St) ->
	Username = get_conn_info(_St, agent_login),
	Db = ouc_db:get_db(imdb),
	{ok, [Agent]} = Db:findOpt(
		<<"entity">>,
		#search{criteria = [
			{<<"ent">>, <<"openacdagent">>},
			{<<"name">>, list_to_binary(Username)}]}, []),
	GetFun = fun(P) -> ej:get(P, Agent, null) end,
	lager:info("Agent json: ~p", [Agent]),
	PermissionProfileName = GetFun({<<"permp">>}),
	lager:info("Getting permission profile for ~p", [PermissionProfileName]),

	{ok, PermissionProfile} = Db:findOpt(
		<<"entity">>,
		#search{criteria = [
			{<<"ent">>, <<"openacdpermissionprofile">>},
			{<<"name">>, PermissionProfileName}
		]}, []),

	Entries = lists:map(
		fun(Props) ->
			Get = fun(Q) -> ej:get(Q, Props, null) end,
			{[
				{monitor, Get({"mon"})},
				{queues, Get({"qs"})},
				{agents, Get({"aggrp"})},
				{barge, Get({"brg"})},
				{entity, Get({"ent"})},
				{name, Get({"name"})},
				{uuid, Get({"uuid"})},
				{type, Get({"type"})},
				{rlm, Get({"rlm"})},
				{custdsk, Get({"custdsk"})},
				{advlg, Get({"advlg"})},
				{trfa, Get({"trfa"})},
				{trfq, Get({"trfq"})},
				{trfn, Get({"trfn"})},
				{confa, Get({"confa"})},
				{confq, Get({"confq"})},
				{confn, Get({"confn"})},
				{chgsktrf, Get({"chgsktrf"})},
				{ctras, Get({"ctras"})},
				{rptab, Get({"rptab"})},
				{sutab, Get({"sutab"})},
				{wdg, Get({"wdg"})}
			]}
		end, PermissionProfile),
		lager:info("Permission profile ~p, ~p", [PermissionProfile, Entries]),
		{[{permissions, Entries}]}.

	%%Get = fun(Q) -> ej:get(Q, Props) end,
	%%Passwd = Get(?PINTOKEN_KEY),
	%%binary_to_list(Passwd).

%% @doc Get live statistics.
%% === Arguments: ===
%% ``` none (internal state)'''
%% === Answer: ===
%% ```
%% {
%%   "live_stats": {
%%     "clients": [{
%%       "name": "client_name",
%%       "stats": {
%%         "calls_in_queue": 1,
%%         "agent_count": {
%%           "total": 0,
%%           "released": 0,
%%           "idle": 1,
%%           "ringing": 0,
%%           "oncall": 0,
%%           "wrapup": 0
%%         }
%%       }
%%     }]
%%   }
%% }'''
%% @end
get_live_stats(St) ->
	APid = cpx_conn_state:get(St, agent_pid),
	Skills = agent:get_skills(APid),
	Clients = proplists:get_all_values('_brand', Skills),
	OnlineAgents = case catch ouc_agents:get_online_agents() of
		Agts when is_list(Agts) ->
			Agts;
		Err ->
			lager:info("ouc_agents:get_online_agents() returned: ~p", [Err]),
			[]
	end,
	Calls = qlc:e(qlc:q([Prop || {_, _, #cpx_gen_media_prop{client=C} = Prop} <- gproc:table({g, p}), lists:any(fun(Client) -> C#client.label =:= Client end, Clients)])),
	TotalCallsInQueue = get_total_calls_in_queue(Calls),
	TotalCounts = get_total_state_counts(Clients, OnlineAgents),
	TotalStats = counts_to_entries(TotalCounts),

	ClientStats = lists:map(fun(Client) ->

    CallsInQueue = get_client_calls_in_queue(Client, Calls),
		AgentCounts = get_client_state_counts(Client, OnlineAgents),
		{[{name, l2b(Client)},
			{stats, {[
				{calls_in_queue, CallsInQueue},
				{agent_count, counts_to_entries(AgentCounts)}
		]}}]}
	end, Clients),
	{[{live_stats, {[{total_calls_in_queue, TotalCallsInQueue}, {total_agent_count, TotalStats}, {clients, ClientStats}]}}]}.

%% @doc Subscribe for the rolling statistics of the current agent.
%% === Arguments: ===
%% ```none (internal state)'''
%% === Answer: ===
%% ```"success" '''
%% @end
get_my_rolling_stats(St) ->
	ouc_rstat:subscribe(),
	Login = cpx_conn_state:get(St, agent_login),
	Clients = cpx_conn_state:get(St, clients),

	ConnPid = self(),
	spawn(fun() ->
		MySnap = ouc_rstat:track_my_client_rstats(Login, Clients, ConnPid),

		MyStatsJson = ouc_rpc_util:my_rstats_to_json(MySnap, Login, Clients),
		ConnPid ! {ouc_my_rolling_stats_result, MyStatsJson}
	end),
	{ok, success}.

%% @doc Get a list of counts of agents matching a set of skills.
%% === Arguments: ===
%% <ul>
%%  <li>`QueueName (string)'</li>
%%  <li>`Skills (JSON array)'</li>
%% </ul>
%% === Answer: ===
%% ```
%% {
%%   "eligible_agents": 2,
%%   "available_agents": 1,
%%   "idle_agents": 0
%% }'''
%% @end
get_matched_agent_count(_St, _Queue, SkillsJson) ->
	Skills = skills_json_to_list(SkillsJson),
	Channels = qlc:e(qlc:q([{Login, State} ||
		{_, _, #cpx_agent_channel_prop{login=Login, state=State}} <- gproc:table({g, p}), State =/= stop])),
	Agents = qlc:e(qlc:q([{Login, AgState} || {_, _, #cpx_agent_prop{login=Login, skills=AgSkills, state=AgState}} <- gproc:table({g, p}),
		lists:member('_all', AgSkills) orelse
		lists:member('_all', Skills) orelse
		util:list_contains_all(AgSkills, Skills)])),
	AvailableAgents = [Login || {Login, AgState} <- Agents, AgState =:= available],
	IdleAgents = [Login || Login <- AvailableAgents, not proplists:is_defined(Login, Channels)],
	{[{eligible_agents, length(Agents)}, {available_agents, length(AvailableAgents)}, {idle_agents, length(IdleAgents)}]}.


%% @doc Sets the dashboard tab layout for the currently connected agent.
%% === Arguments: ===
%% <ul>
%%  <li> `Tab' </li>
%%  <li> `Width' </li>
%%  <li> `Gadgets' </li>
%% </ul>
%% === Answer: ===
%% `"success"'
%% @end
set_tab_layout(St, Tab, Width, Gadgets) ->
	User = get_conn_info(St, agent_login),
	lager:debug("Setting tab layout for user ~p with arguments ~p", [User, [Tab, Width, Gadgets]]),
	set_user_layout(User, Tab, Width, Gadgets),
	{ok, success}.

%% @doc Initiate an outbound call using a client as argument.
%% === Arguments ===
%% `Properties (JSON array)'
%% ```
%% {
%%  "client": "ClientName", //mandatory
%%  ...
%% }'''
%% === Answer ===
%% `` "success" '' or
%% ```
%% {"error": "Reason Description"}'''
%% @end
initiate_outbound(St, Props) ->
	AgentLogin = get_conn_info(St, agent_login),
	Apid = cpx_conn_state:get(St, agent_pid),
	lager:notice("In ouc_rpc:initiate outbound ~p", [Props]),
	ClientBin = ej:get({"client"}, Props, undefined),

	try binary_to_list(ClientBin) of
		Client ->
			Reply = freeswitch_media_manager:initiate_outbound_call({AgentLogin, Apid}, Client),
			case Reply of
				{error, Reason} -> {error, 2, Reason};
				{ok, Pid} ->
					St2 = cpx_conn_state:set_prop(St, outbound_pid, Pid),
					{ok, success, St2}
			end
	catch
		error:_Reason -> {error, 2, <<"destination must be string">>};
		Error -> lager:error("Failed with ~p", [Error])
	end.

%% @doc Initiate an outbound call.
%% === Arguments: ===
%% `Destination'
%% === Answer: ===
%% `"success"' or
%% ```
%% {"error": "Error reason"}'''
%% @end
call_outbound(St, DestBin) ->
	_AgentLogin = get_conn_info(St, agent_login),
	Apid = cpx_conn_state:get(St, agent_pid),
	case cpx_conn_state:get_prop(St, outbound_pid) of
		error -> {error, 2, <<"outbound pid missing">>};
		{ok, Pid} ->
					try binary_to_list(DestBin) of
						%% TODO change 2nd Apid to Pid of stored outbound_call
						Dest -> Reply = freeswitch_media_manager:make_outbound_call(Apid, Pid, Dest),
								case Reply of
									{error, Reason} -> {error, 2, Reason};
									ok -> {ok, success}
								end
					catch
						error:_Reason -> {error, 2, <<"destination must be string">>};
						Error -> lager:error("Failed with ~p", [Error])
					end
	end.

%% @doc Hold the current outbound call (??).
%% === Arguments: ===
%% `none (internal state)'
%% === Answer: ===
%% `"ok"' or
%% ```
%% { "error": "Outbound PID missing"}'''
%% @end
hold_outbound(St) ->
	case cpx_conn_state:get_prop(St, outbound_pid) of
		error -> {error, 2, <<"outbound pid missing">>};
		{ok, Pid} ->
			freeswitch_media_manager:hold_outbound(Pid)
	end.

%% @doc UnHold the current outbound call (??).
%% === Arguments: ===
%% `none (internal state)'
%% === Answer: ===
%% `"ok"' or
%% ```
%% { "error": "Outbound PID missing"}'''
%% @end
unhold_outbound(St) ->
	case cpx_conn_state:get_prop(St, outbound_pid) of
		error -> {error, 2, <<"outbound pid missing">>};
		{ok, Pid} ->
			freeswitch_media_manager:unhold_outbound(Pid)
	end.

%% @doc Hangup the current outbound call (??).
%% === Arguments: ===
%% `none (internal state)'
%% === Answer: ===
%% `"ok"' or
%% ```
%% { "error": "Outbound PID missing"}'''
%% @end
hangup_outbound(St) ->
	case cpx_conn_state:get_prop(St, outbound_pid) of
		error -> {error, 2, <<"outbound pid missing">>};
		{ok, Pid} ->
			freeswitch_media_manager:stop_outbound(Pid)
	end.

%% @doc Transfer the current outbound call to a given destination(??).
%% === Arguments: ===
%% `Destination (string)'
%% === Answer: ===
%% `"ok" '
%% or
%% ```
%% {"error": "Outbound PID missing"}'''
%% @end
transfer_outbound(St, Dest) ->
	case cpx_conn_state:get_prop(St, outbound_pid) of
		error ->
			{error, 2, <<"outbound pid missing">>};
		{ok, Pid} ->
			freeswitch_outbound:transfer(Pid, b2l(Dest))
	end.

%% @doc Initiates an outbound conference with the current agent to a given destination.
%% === Arguments: ===
%% `Destination (string)'
%% === Answer: ===
%% `"ok" '
%% or
%% ```
%% {"error": "Outbound PID missing"}'''
%% @end
conference_outbound(St, Dest) ->
	case cpx_conn_state:get_prop(St, outbound_pid) of
		error ->
			{error, 2, <<"outbound pid missing">>};
		{ok, Pid} ->
			freeswitch_outbound:conference(Pid, b2l(Dest))
	end.

%% @doc Sends an outbound call to voicemail.
%% === Arguments: ===
%% <ul>
%%  <li> `ChannelID (string)'</li>
%%  <li> `Destination (string)' </li>
%% </ul>
%% === Answer: ===
%% `"success" '
%% or
%% ```
%% {"error": "Destination must be string"}''' or
%% ```
%% {"error": "Failed with <error>"}'''
%% @end
call_voicemail_outbound(St, ChanId, DestBin) ->
	with_channel_do(St, ChanId, fun(ChanPid) ->
		try binary_to_list(DestBin) of
			%% TODO change 2nd Apid to Pid of stored outbound_call
			Dest -> lager:debug("voicemail destination ~p", [Dest]),
					Reply = agent_channel:make_voicemail_outbound(ChanPid, Dest),
					lager:debug("voicemail make outbound reply ~p",[Reply]),
					{ok, success}
		catch
			error:_Reason -> {error, 2, <<"destination must be string">>};
			Error -> lager:error("Failed with ~p", [Error])
		end
	end).

%% @doc Get transfer agents (??).
%% === Arguments: ===
%% `none (internal string)'
%% === Answer: ===
%% ```{
%%   "transfer_agents": {
%%     "1234": {
%%       "profile": "Default",
%%       "first_name": "Bob",
%%       "last_name": "Builder",
%%       "state": "released"
%%     }
%%   }
%% }'''
%% @end
get_transfer_agents(St) ->
	User = get_conn_info(St, agent_login),
	Agents = ouc_agents:get_transfer_agents(User),
	Resp = {[
		{transfer_agents, lists:map(fun(A) ->
		{l2b(proplists:get_value(login, A)), {[
		{profile, l2b(proplists:get_value(profile, A))},
		{first_name, nob(proplists:get_value(first_name, A))},
		{last_name, nob(proplists:get_value(last_name, A))},
		{state, proplists:get_value(state, A)}]}}
	end, Agents)}]},
	{ok, Resp}.

%% @doc Subscribe and get transfer agents (??).
%% === Arguments: ===
%% `none (internal state)'
%% === Answer: ===
%% ```
%% {
%%   "transfer_agents": {
%%     "1234": {
%%       "profile": "Default",
%%       "first_name": "Bob",
%%       "last_name": "Builder",
%%       "state": "released"
%%     }
%%   }
%% }'''
%% @end
subscribe_transfer_agents(St) ->
	ouc_agents:subscribe_transfer_agents(),
	get_transfer_agents(St).

%% @doc Unsubscribe transfer agents.
%% === Arguments: ===
%% `none (internal state)'
%% === Answer: ===
%% `"success"'
%% @see ouc_rpc:subscribe_transfer_agents/1
unsubscribe_transfer_agents(_St) ->
	ouc_agents:unsubscribe_transfer_agents(),
	{ok, success}.

%% @doc Get a list of websocket URL endpoints.
%% === Arguments: ===
%% `none (internal state)'
%% === Answer: ===
%% ```
%% {
%%  "wss": "wss://openuc-domain.com:8937/wsock",
%%  "ws" : "ws://openuc-domain.com:8936/wsock"
%% }
%% or
%% {
%%  "wss": "",
%%  "ws" : ""
%% }'''
%% @end
get_wsock_urls(_St) ->
	case application:get_env(reach_core, nodes) of
		{ok, Nodes} ->
			{WsUrls, WssUrls} = lists:foldl(fun(N, {Ws, Wss}) ->
				NStr = atom_to_list(N),
				[_, Domain] = string:tokens(NStr, "@"),
				Url1 = l2b("ws://" ++ Domain ++ ":8937/wsock"),
				Url2 = l2b("wss://" ++ Domain ++ ":8936/wsock"),
				{[Url1|Ws], [Url2|Wss]}
			end, {[], []}, Nodes),
			{ok, {[{wss, WssUrls}, {ws, WsUrls}]}};
		_ ->
			{ok, {[{wss, []}, {ws, []}]}}
	end.

% Internal functions

with_channel_do(St, ChanIdBin, Fun) ->
	AgentLogin = cpx_conn_state:get(St, agent_login),
	ChanId = binary_to_list(ChanIdBin),
	case agent_state_manager:get_channel(AgentLogin, ChanId) of
		ChanPid when is_pid(ChanPid) ->
			Fun(ChanPid);
		_ ->
			err(channel_not_found)
	end.

err(invalid_rel) ->
	{error, 4001, <<"Invalid/Missing release code">>};
err(channel_not_found) ->
	{error, 4002, <<"Channel not found">>};
err(invalid_state_change) ->
	{error, 4003, <<"Invalid state change">>}.

get_contact_info_struct(User, Password) ->
	{ok, ConfigApi} = application:get_env(reach_ouc, sipxconfig_rest_api),
	{ok, ContactInfoResource} = application:get_env(reach_ouc, contact_info_resource),
	ContactInfoUrl = ConfigApi ++ ContactInfoResource,

	AuthHeader = basic_auth_header(User, Password),
	lager:debug("Calling GET on ~p", [ContactInfoUrl]),
	case http_get(ContactInfoUrl, [AuthHeader, accept_json_header()]) of
		{ok, "200", _, ResponseBody} ->
			{ok, ejrpc2_json:decode(ResponseBody, [{format, eep18}])};
		_ ->
			{error, http_get}
	end.

set_contact_info_struct(Struct, User, Password) ->
	{ok, ConfigApi} = application:get_env(reach_ouc, sipxconfig_rest_api),
	{ok, ContactInfoResource} = application:get_env(reach_ouc, contact_info_resource),
	ContactInfoUrl = ConfigApi ++ ContactInfoResource,

	AuthHeader = basic_auth_header(User, Password),
	lager:debug("Calling PUT on ~p", [ContactInfoUrl]),
	case http_put(ContactInfoUrl, [AuthHeader, content_json_header()], Struct) of
		{ok, "200", _, _} ->
			{ok, success};
		Error ->
			lager:debug("Set contact info error ~p", [Error]),
			{error, http_put}
	end.

build_contact_info_struct(Map, Old, New) ->
	lists:foldl(fun({FromKey, ToKey}, Acc) ->
		case ej:get(FromKey, New, null) of
			null ->
				Acc;
			Val ->
				ej:set(ToKey, Acc, Val)
		end
	end, Old, Map).

http_get(Url, Headers) ->
	ibrowse:send_req(Url, Headers, get).

http_put(Url, Headers, Body) ->
	ibrowse:send_req(Url, Headers, put, Body).

basic_auth_header(User, Password) ->
	{"Authorization", "Basic " ++ ibrowse_lib:encode_base64(User ++ ":" ++ Password)}.

accept_json_header() ->
	{"Accept", "application/json"}.

content_json_header() ->
	{"Content-Type", "application/json"}.

json_build_map() ->
 	[{{"phone", "direct"}, {"contact-information", "didNumber"}},
	{{"phone", "cell"}, {"contact-information", "cellPhoneNumber"}},
	{{"email"}, {"contact-information", "emailAddress"}},
	{{"home_address", "street"}, {"contact-information", "homeAddress", "street"}},
	{{"home_address", "city"}, {"contact-information", "homeAddress", "city"}},
	{{"home_address", "state"}, {"contact-information", "homeAddress", "state"}},
	{{"home_address", "country"}, {"contact-information", "homeAddress", "country"}},
	{{"home_address", "zip"}, {"contact-information", "homeAddress", "zip"}}].

get_conn_info(St, Key) ->
	cpx_conn_state:get(St, Key).

get_user_password(User) ->
	Db = ouc_db:get_db(imdb),
	{ok, Props} = Db:findOne(<<"entity">>, [{<<"type">>, <<"openacdagent">>}, {<<"name">>, User}]),
	Get = fun(Q) -> ej:get(Q, Props) end,
	Passwd = Get(?PINTOKEN_KEY),
	binary_to_list(Passwd).

encode_json(In) ->
	ejrpc2_json:encode(In).

get_total_calls_in_queue(Calls) ->
	CIQ = [State || #cpx_gen_media_prop{state=State} <- Calls,
		lists:member(State, ?INQUEUE_STATES)],
	length(CIQ).

get_client_calls_in_queue(Client, Calls) ->
	CIQ = [State || #cpx_gen_media_prop{client=#client{label=CallClient}, state=State} <- Calls,
		CallClient =:= Client, lists:member(State, ?INQUEUE_STATES)],
	length(CIQ).

get_total_state_counts(Clients, OnlineAgents) ->
	AgentChannels = [{Channels, State} || #online_agent{skills=Skills, state=State, active_channels=Channels} <- OnlineAgents, lists:any(fun(C) -> lists:member({'_brand', C}, Skills) end, Clients)],
	lists:foldl(fun({[], State}, #client_counts{idle=Idle}=Acc) ->
			Acc#client_counts{idle = increase_count(State, Idle)};
		({[#active_channel{state=ringing}|_], State}, #client_counts{ringing=Ringing}=Acc) ->
			Acc#client_counts{ringing = increase_count(State, Ringing)};
		({[#active_channel{state=oncall}|_], State}, #client_counts{oncall=Oncall}=Acc) ->
			Acc#client_counts{oncall = increase_count(State, Oncall)};
		({[#active_channel{state=wrapup}|_], State}, #client_counts{wrapup=Wrapup}=Acc) ->
			Acc#client_counts{wrapup = increase_count(State, Wrapup)};
		(_, Acc) -> Acc
	end, #client_counts{}, AgentChannels).

get_client_state_counts(Client, OnlineAgents) ->
	AgentChannels = [{Channels, State} || #online_agent{skills=Skills, state=State, active_channels=Channels} <- OnlineAgents, lists:member({'_brand', Client}, Skills)],
	lists:foldl(fun({[], State}, #client_counts{idle=Idle}=Acc) ->
			Acc#client_counts{idle = increase_count(State, Idle)};
		({[#active_channel{state=ringing}|_], State}, #client_counts{ringing=Ringing}=Acc) ->
			Acc#client_counts{ringing = increase_count(State, Ringing)};
		({[#active_channel{state=oncall}|_], State}, #client_counts{oncall=Oncall}=Acc) ->
			Acc#client_counts{oncall = increase_count(State, Oncall)};
		({[#active_channel{state=wrapup}|_], State}, #client_counts{wrapup=Wrapup}=Acc) ->
			Acc#client_counts{wrapup = increase_count(State, Wrapup)};
		(_, Acc) -> Acc
	end, #client_counts{}, AgentChannels).

increase_count(available, {R,A}) ->
	{R, A+1};
increase_count(_, {R,A}) ->
	{R+1, A}.

counts_to_entries(Counts) ->
	{[
		{idle, relavl_entry(Counts#client_counts.idle)},
		{ringing, relavl_entry(Counts#client_counts.ringing)},
		{oncall, relavl_entry(Counts#client_counts.oncall)},
		{wrapup, relavl_entry(Counts#client_counts.wrapup)}
	]}.

relavl_entry({R, A}) ->
	{[{released, R}, {available, A}]}.

set_user_layout(User, Tab, Width, Gadgets) ->
	Db = ouc_db:get_db(openacd),
	Doc = case Db:findOne(<<"user_layout">>, [{<<"agent">>, User}]) of
		{ok, Entry} when length(Entry) > 0 ->
			NewTab = {Tab, [{<<"gadgets">>, {array, form_gadgets(Gadgets)}}, {<<"layout">>, Width}]},
			TabList = ej:get({"tabs"}, Entry),
			TabListUpdate = case proplists:get_value(Tab, TabList) of
				undefined ->
					[NewTab | TabList];
				_ ->
					lists:keyreplace(Tab, 1, TabList, NewTab)
			end,
			lists:keyreplace(<<"tabs">>, 1, Entry, {<<"tabs">>, TabListUpdate});
		_ ->
			[{<<"agent">>,list_to_binary(User)},
			{<<"tabs">>, [{Tab, [{<<"gadgets">>, {array, form_gadgets(Gadgets)}}, {<<"layout">>,Width}]}]}]
	end,
	Db:update(<<"user_layout">>, [{<<"agent">>, User}], Doc, [upsert]),
	Doc.

form_gadgets(Conf) ->
	lists:map(fun({Gadget}) ->
		Name = proplists:get_value(<<"name">>, Gadget),
		Position = proplists:get_value(<<"position">>, Gadget),
		Width = proplists:get_value(<<"width">>, Gadget),
		[{<<"name">>, Name}, {<<"position">>, {array, Position}}, {<<"width">>, Width}]
	end, Conf).

skills_json_to_list(SkillsJson) ->
	lists:foldl(fun(S, Acc) when is_binary(S) ->
			[binary_to_existing_atom(S, utf8) | Acc];
		({[{<<"client">>, ClientBin}]}, Acc) ->
			[{'_brand', b2l(ClientBin)} | Acc];
		(_, Acc) ->
			Acc
	end, [], SkillsJson).

%% TESTS


-ifdef(TEST).
-include_lib("reach_core/include/agent.hrl").

t_media(Client, State) ->
	spawn_link(fun() ->
		Prop = #cpx_gen_media_prop{client=#client{label=Client}, state=State},
		gproc:add_global_property(cpx_gen_media, Prop),
		receive _ -> ok end
	end).

get_client_stats_test_() ->
	{setup, fun() ->
		application:start(gproc),
		meck:new(ouc_agents),
		meck:new(agent)
	end, fun(_) ->
		meck:unload(),
		application:stop(gproc)
	end, [{"get client stats", fun() ->
		meck:expect(agent, get_skills, 1, [{'_brand', "Client 1"}, {'_brand', "Client 2"}]),
		meck:expect(ouc_agents, get_online_agents, 0, [
			#online_agent{skills=[{'_brand', "Client 1"}], state=available, active_channels=[#active_channel{state=ringing}]},
			#online_agent{skills=[{'_brand', "Client 2"}], state=available, active_channels=[#active_channel{state=oncall}]},
			#online_agent{skills=[{'_brand', "Client 1"}], state={released, {"rel0", "Lunch", 0}}, active_channels=[#active_channel{state=wrapup}]},
			#online_agent{skills=[{'_brand', "Client 2"}], state={released, {"rel0", "Lunch", 0}}, active_channels=[#active_channel{state=wrapup}]},
			#online_agent{skills=[{'_brand', "Client 1"}], state=available},
			#online_agent{skills=[{'_brand', "Client 2"}], state={released, {"rel0", "Lunch", 0}}}
		]),
		t_media("Client 1", inivr),
		t_media("Client 1", inivr),
		t_media("Client 1", inqueue),
		t_media("Client 1", ringing),
		t_media("Client 2", inivr),
		t_media("Client 2", inivr),
		t_media("Client 2", wrapup),

		?assertEqual(
			{[{live_stats, {[
				{total_calls_in_queue, 5},
				{total_agent_count,
							{[{idle, {[{released,1},{available,1}]}},
							{ringing, {[{released,0},{available,1}]}},
							{oncall, {[{released,0},{available,1}]}},
							{wrapup, {[{released,2},{available,0}]}}]}},
				{clients, [{[{name,<<"Client 1">>},
					{stats,
						{[{calls_in_queue,3},
						{agent_count,
							{[{idle, {[{released,0},{available,1}]}},
							{ringing, {[{released,0},{available,1}]}},
							{oncall, {[{released,0},{available,0}]}},
							{wrapup, {[{released,1},{available,0}]}}]}}]}}]},
					{[{name,<<"Client 2">>},
					{stats,
						{[{calls_in_queue,2},
						{agent_count,
							{[{idle, {[{released,1}, {available,0}]}},
							{ringing, {[{released,0}, {available,0}]}},
							{oncall, {[{released,0},{available,1}]}},
							{wrapup, {[{released,1},{available,0}]}}]}}
		]}}]}]}]}}]},
		get_live_stats(t_st()))
	end}]}.

get_contact_info_test_() ->
	{setup, fun() ->
		meck:new(mongoapi),
		meck:expect(mongoapi, new, fun(N, Db) -> {mongoapi, N, Db} end)
	end, fun(_) ->
		meck:unload(mongoapi)
	end, [fun() ->
		meck:expect(mongoapi, findOne,
			fun(<<"userProfile">>, [{<<"m_userName">>, "1000"}], _) ->
				{ok, t_samp_profile()}
			end),


		?assertEqual(
			{[{username, <<"1000">>},
			{first_name, <<"first1000">>},
			{last_name, <<"last1000">>},
			{email, <<"1000@oacddev.ezuce.com">>},
			{home_address, {[
				{street, <<"home street 1000">>},
				{city, <<"home city 1000">>},
				{state, <<"home state 1000">>},
				{country, <<"home country 1000">>},
				{zip, <<"home zip 1000">>}
				]}},
			{phone, {[
				{internal, <<"1000">>},
				{cell, <<"cell 1000">>},
				{direct, <<"did 1000">>}
			]}},
			{position, <<"JbTtl 1000">>},
			{avatar, <<"https://oacddev.ezuce.com/sipxconfig/rest/avatar/1000">>},
			{manager, <<"manager1000">>}]},
			get_contact_info(t_st()))
	end]}.

get_lines_test_() ->
	{setup, fun() ->
		meck:new(mongoapi),

		meck:expect(mongoapi, new, fun(N, Db) -> {mongoapi, N, Db} end),
		meck:expect(mongoapi, findOpt, 4, {ok, t_samp_lines()})
	end, fun(_) ->
		meck:unload()
	end, [{"get lines", fun() ->
		?assertEqual({[{lines, [
			{[{name, <<"Line6">>}, {extension, <<"6">>}, {did_number, <<"7000006">>}, {client, <<"Client 1">>}]},
           	{[{name, <<"Line7">>}, {extension, <<"7">>}, {did_number, <<"7000007">>}, {client, <<"Client 1">>}]}]}]},
       	get_lines(t_st()))
	end}]}.

get_clients_test_() ->
	{setup, fun() ->
		meck:new(mongoapi),

		meck:expect(mongoapi, new, fun(N, Db) -> {mongoapi, N, Db} end),
		meck:expect(mongoapi, findOne, fun(_,[_,{<<"name">>,Name}],_) ->
			{ok, t_samp_client(Name)} end),
		meck:expect(mongoapi, findOpt, 4, {ok, t_samp_clients()})
	end, fun(_) ->
		meck:unload()
	end, [
	{"get clients", fun() ->
		?assertEqual([{[{name,<<"Client1">>},
			{avatar,<<"http://oacddev.ezuce.com/Client1.jpg">>},
			{urlpop,<<"http://oacddev.ezuce.com/customer_history/{callerid}">>},
			{call_dispositions,{array,[<<"Resolved">>, <<"To follow up">>]}}]},
		{[{name,<<"Client2">>},
			{avatar,<<"http://oacddev.ezuce.com/Client2.jpg">>},
			{urlpop,null},
			{call_dispositions,{array,[]}}]}],
		get_clients(t_st()))
	end},
	{"get client", fun() ->
		?assert(true),
		?assertEqual({[{name,<<"Client1">>},
			{avatar,<<"http://oacddev.ezuce.com/Client1.jpg">>},
			{urlpop,<<"http://oacddev.ezuce.com/customer_history/{callerid}">>},
			{call_dispositions,{array,[<<"Resolved">>, <<"To follow up">>]}}]},
		get_client(t_st(), <<"Client1">>)),
		?assertEqual({[{name,<<"Client2">>},
			{avatar,<<"http://oacddev.ezuce.com/Client2.jpg">>},
			{urlpop,null},
			{call_dispositions,{array,[]}}]},
		get_client(t_st(), <<"Client2">>))
	end}]}.

set_layout_test_() ->
	{setup, fun() ->
		meck:new(mongoapi),

		meck:expect(mongoapi, new, fun(N, Db) -> {mongoapi, N, Db} end),
		meck:expect(mongoapi, findOne, 3, {ok, []}),
		meck:expect(mongoapi, update, 5, ok)
	end, fun(_) ->
		meck:unload()
	end, [{"get lines", fun() ->
		?assertEqual({ok, success}, set_tab_layout(t_st(), <<"maintab">>, <<"narrow">>, [{[{<<"name">>,<<"maingadget">>},{<<"position">>,[1,1]},{<<"width">>,1}]}])),
		?assert(meck:called(mongoapi, update, [<<"user_layout">>, [{<<"agent">>,"1000"}],
			[{<<"agent">>,<<"1000">>},
			 {<<"tabs">>,
				[{<<"maintab">>,
					[{<<"gadgets">>, {array, [[{<<"name">>, <<"maingadget">>},
						{<<"position">>, {array,[1,1]}},
						{<<"width">>, 1}]]}},
					 {<<"layout">>, <<"narrow">>}]}]}],
			[upsert],
			{mongoapi,ouc,<<"openacd">>}]))
	end}]}.

t_samp_lines() ->
	[[{<<"_id">>,<<"OpenAcdLine6">>},
		{<<"ident">>,<<"6@oacddev.ezuce.com">>},
		{<<"ent">>,<<"openacdline">>},
		{<<"linm">>,<<"Line6">>},
		{<<"did">>,<<"7000006">>},
		{<<"clnm">>,<<"Client 1">>}],
	[{<<"_id">>,<<"OpenAcdLine7">>},
		{<<"ident">>,<<"7@oacddev.ezuce.com">>},
		{<<"ent">>,<<"openacdline">>},
		{<<"linm">>,<<"Line7">>},
		{<<"did">>,<<"7000007">>},
		{<<"clnm">>,<<"Client 1">>}]].

t_samp_clients() ->
	[t_samp_client(<<"Client1">>), t_samp_client(<<"Client2">>)].
t_samp_client(<<"Client1">>) ->
	[{<<"_id">>,<<"OpenAcdClient1">>},
		{<<"useExtAvt">>,false},
		{<<"ident">>,<<"Client1">>},
		{<<"call_dispositions">>,
			{array,[<<"Resolved">>,<<"To follow up">>]}},
		{<<"ent">>,<<"openacdclient">>},
		{<<"name">>,<<"Client1">>},
		{<<"uuid">>,<<"uuid">>},
		{<<"type">>,<<"openacdclient">>},
		{<<"avt">>,<<"http://oacddev.ezuce.com/Client1.jpg">>},
		{<<"urlpop">>,
			<<"http://oacddev.ezuce.com/customer_history/{callerid}">>}];
t_samp_client(<<"Client2">>) ->
	[{<<"_id">>,<<"OpenAcdClient2">>},
		{<<"useExtAvt">>,true},
		{<<"ident">>,<<"Client2">>},
		{<<"call_dispositions">>,{array,[]}},
		{<<"ent">>,<<"openacdclient">>},
		{<<"name">>,<<"Client2">>},
		{<<"uuid">>,<<"uuid">>},
		{<<"type">>,<<"openacdclient">>},
		{<<"extAvt">>, <<"http://oacddev.ezuce.com/Client2.jpg">>}].

t_st() ->
	cpx_conn_state:new(#agent{login="1000"}).

t_samp_profile() ->
	[{<<"_id">>,<<"11">>},
	 {<<"_class">>,
	  <<"org.sipfoundry.commons.userdb.profile.UserProfile">>},
	 {<<"m_userName">>,<<"1000">>},
	 {<<"m_firstName">>,<<"first1000">>},
	 {<<"m_lastName">>,<<"last1000">>},
	 {<<"m_jobTitle">>,<<"JbTtl 1000">>},
	 {<<"m_jobDept">>,<<"Dept 1000">>},
	 {<<"m_companyName">>,<<"Company 1000">>},
	 {<<"m_assistantName">>,<<"Asst. 1000">>},
	 {<<"m_location">>,<<"Loc 1000">>},
	 {<<"m_homeAddress">>,
	  [{<<"m_street">>,<<"home street 1000">>},
	   {<<"m_city">>,<<"home city 1000">>},
	   {<<"m_country">>,<<"home country 1000">>},
	   {<<"m_state">>,<<"home state 1000">>},
	   {<<"m_zip">>,<<"home zip 1000">>}]},
	 {<<"m_officeAddress">>,
	  [{<<"m_street">>,<<"ofc street 1000">>},
	   {<<"m_city">>,<<"ofc city 1000">>},
	   {<<"m_country">>,<<"ofc country 1000">>},
	   {<<"m_state">>,<<"ofc state 1000">>},
	   {<<"m_zip">>,<<"ofc zip 1000">>},
	   {<<"m_officeDesignation">>,<<"ofc desig 1000">>}]},
	 {<<"m_cellPhoneNumber">>,<<"cell 1000">>},
	 {<<"m_homePhoneNumber">>,<<"homephn 1000">>},
	 {<<"m_assistantPhoneNumber">>,<<"asstphn 1000">>},
	 {<<"m_faxNumber">>,<<"fax 1000">>},
	 {<<"m_didNumber">>,<<"did 1000">>},
	 {<<"m_imId">>,<<"1000">>},
	 {<<"m_alternateImId">>,<<"altim 1000">>},
	 {<<"m_emailAddress">>,<<"1000@oacddev.ezuce.com">>},
	 {<<"m_alternateEmailAddress">>,<<"alt1000@a.com">>},
	 {<<"m_useBranchAddress">>,false},
	 {<<"m_manager">>,<<"manager1000">>},
	 {<<"m_salutation">>,<<"None">>},
	 {<<"m_employeeId">>,<<"emp1000">>},
	 {<<"m_twiterName">>,<<"twitter1000">>},
	 {<<"m_linkedinName">>,<<"linkedin1000">>},
	 {<<"m_facebookName">>,<<"fb1000">>},
	 {<<"m_xingName">>,<<"xing1000">>},
	 {<<"m_avatar">>,
	  <<"https://oacddev.ezuce.com/sipxconfig/rest/avatar/1000">>},
	 {<<"m_useExtAvatar">>,false}].

-endif.
