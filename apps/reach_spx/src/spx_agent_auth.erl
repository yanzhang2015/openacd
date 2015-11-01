%% Copyright (c) 2010 / 2011 eZuce, Inc. All rights reserved.
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

-module(spx_agent_auth).

-include_lib("reach_core/include/agent.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% -ifdef(TEST).
% -export([reset_test_db/0]).
% -define(DB, <<"imdb_test">>).
% -else.
-define(DB, <<"imdb">>).
% -endif.

-behaviour(agent_auth).

%% API
-export([
	start/0
]).

%% Callbacks
-export([
	get_agent_by_login/1,
	get_agent_by_id/1,
	get_agents_by_profile/1,
	auth/2,
	get_profile/1,
	get_default_profile/0,
	get_profiles/0,
	get_release/1,
	get_releases/0,
	has_permission/2
]).

%% Utils
-export([
	get_agents/0,
	get_license_seats/0
]).

%%====================================================================
%% API
%%====================================================================

start() ->
	%% Agents

	%% Hooks for use with multi auth
	cpx_hooks:set_hook(spx_get_agents, get_agents, ?MODULE, get_agents, [], 200),
	cpx_hooks:set_hook(spx_get_agents_by_profile, get_agents_by_profile, ?MODULE, get_agents_by_profile, [], 200),
	cpx_hooks:set_hook(spx_get_agent, get_agent, ?MODULE, get_agent, [], 200),
	cpx_hooks:set_hook(spx_auth_agent, auth_agent, ?MODULE, auth_agent, [], 200),
	cpx_hooks:set_hook(spx_get_profiles, get_profiles, ?MODULE, get_profiles, [], 200),
	cpx_hooks:set_hook(spx_get_profile, get_profile, ?MODULE, get_profile, [], 200),
	cpx_hooks:set_hook(spx_get_releases, get_releases, ?MODULE, get_releases, [], 200),

	ok.

%% Callbacks

%% @doc Get all agents. Use only for testing
-spec(get_agents/0 :: () -> {ok, [#agent_auth{}]}).
get_agents() ->
	case catch db_find(agent, []) of
		{ok, AgentProps} ->
			{ok, [X || P <- AgentProps, {ok, X} <- [spx_util:build_agent(P)]]};
		_ ->
			{ok, []}
	end.

get_agents_by_profile(Profile) ->
	case catch db_find(agent, [{<<"aggrp">>, iolist_to_binary(Profile)}]) of
		{ok, AgentProps} ->
			{ok, [X || P <- AgentProps, {ok, X} <- [spx_util:build_agent(P)]]};
		_ ->
			{ok, []}
	end.

get_agent_by_login(Login) ->
	case catch db_find_one(agent, [{<<"name">>, Login}]) of
		{ok, []} -> none;
		{ok, P} -> spx_util:build_agent(P);
		_ -> none
	end.

get_agent_by_id(ID) ->
	case catch db_find_one(agent, [{<<"_id">>, ID}]) of
		{ok, []} -> none;
		{ok, P} -> spx_util:build_agent(P);
		_ -> none
	end.

-spec auth(Username::string(), Password::string()) -> {ok, #agent_auth{}} | {ok, deny} | pass | {error, not_connected}.
auth(Username, Password) ->
	case catch db_find_one(agent, [{<<"name">>, Username}]) of
		{ok, []} -> pass;
		{ok, P} ->
			%UsernameBin = list_to_binary(Username),
			PasswordBin = list_to_binary(Password),
			%Realm = proplists:get_value(<<"rlm">>, P, <<>>),

			%DigestBin = crypto:md5(<<UsernameBin/binary, $:, Realm/binary, $:, PasswordBin/binary>>),
			%DigestHexBin = iolist_to_binary([io_lib:format("~2.16.0b", [C]) || <<C>> <= DigestBin]),

			PntkHexBin = proplists:get_value(<<"pntk">>, P, <<>>),
			case PntkHexBin of
				PasswordBin ->
					spx_util:build_agent(P);
				_ ->
					{ok, deny}
			end;
		not_connected -> {error, not_connected};
		_ -> pass
	end.

get_profiles() ->
	case db_find(profile, []) of
		{ok, Props} ->
			{ok, [X || P <- Props, {ok, X} <- [spx_util:build_profile(P)]]};
		_ ->
			{ok, []}
	end.

get_profile(Profile) ->
	case catch db_find_one(profile, [{<<"name">>, Profile}]) of
		{ok, []} -> undefined;
		{ok, P} ->
			spx_util:build_profile(P);
		_ -> undefined
	end.

get_default_profile() ->
	get_profile("Default").

get_release(ID) ->
	case catch db_find_one(release_opt, [{<<"_id">>, ID}]) of
		{ok, []} -> none;
		{ok, P} -> spx_util:build_release_opt(P);
		_ -> none
	end.

get_releases() ->
	case catch db_find(release_opt, []) of
		{ok, Props} ->
			{ok, [R || P <- Props, {ok, R} <- [spx_util:build_release_opt(P)]]};
		_ ->
			{ok, []}
	end.

get_license_seats() ->
  case catch db_find_one(settings, []) of
    {ok, []} -> none;
    {ok, Props} ->
      lager:info("Result from database: ~p", [Props]),
      spx_util:get_license_seats(Props);
    _ -> none
  end.

-spec has_permission(PermProfile :: atom(), PermName :: tuple() | binary()) -> {ok, true} | {ok, false} | {error, any}.
%% @doc Check if a permission is enabled in a certain permission profile.
%%  PermKey is either a binary string (for boolean permissions) or a tuple
%%  {key, value}, where key identifies the permission and the value
%%  corresponds to a certain value belonging to that permission (for array
%%  permissions, like <<"wdg">>).
has_permission(PermProfile, PermKey) ->
  {PermName, Value} = case PermKey of
    {K, V} -> {K, V};
    Other  -> {Other, undefined}
  end,
  case catch db_find_one(permission_profile,
      [{<<"name">>, atom_to_binary(PermProfile, utf8)}],
      [{PermName, 1}, {<<"_id">>, 0}]) of
    {ok, []} -> {ok, false};
    {ok, [Prop]} ->
      case Prop of
        {PermName, {array, List}} when Value /= undefined ->
          {ok, lists:member(Value, List)};
        {PermName, Bool} when is_boolean(Bool) -> {ok, Bool};
        _ -> {ok, false}
      end
  end.

%% Internal functions
db_find(agent, Props) ->
	db_find(<<"openacdagent">>, Props);
db_find(settings, Props) ->
  db_find(<<"openacdsettings">>, Props);
db_find(permission_profile, Props) ->
  db_find(<<"openacdpermissionprofile">>, Props);
db_find(profile, Props) ->
	db_find(<<"openacdagentgroup">>, Props);
db_find(release_opt, Props) ->
	db_find(<<"openacdreleasecode">>, Props);
db_find(Type, Props) when is_binary(Type) ->
	db_find([{<<"type">>, Type}|Props]).

db_find(Props) when is_list(Props) ->
	DB = mongoapi:new(spx, ?DB),
	DB:find(<<"entity">>, Props,
		undefined, 0, 0).
db_find_one(agent, Props) ->
	db_find_one(<<"openacdagent">>, Props);
db_find_one(settings, Props) ->
	db_find_one(<<"openacdsettings">>, Props);
db_find_one(profile, Props) ->
	db_find_one(<<"openacdagentgroup">>, Props);
db_find_one(release_opt, Props) ->
	db_find_one(<<"openacdreleasecode">>, Props);
db_find_one(permission_profile, Props) ->
  db_find_one(<<"openacdpermissionprofile">>, Props);
db_find_one(Type, Props) when is_binary(Type) ->
	db_find_one([{<<"type">>, Type}|Props]).
db_find_one(Props) when is_list(Props) ->
	DB = mongoapi:new(spx, ?DB),
	DB:findOne(<<"entity">>, Props).

db_find_one(permission_profile, Props, Opts) ->
  db_find_one(<<"openacdpermissionprofile">>, Props, Opts);
db_find_one(Type, Props, Opts) when is_binary(Type) ->
  DB = mongoapi:new(spx, ?DB),
  DB:findOne(<<"entity">>, [{<<"type">>, Type} | Props], Opts).

-ifdef(TEST).
%%--------------------------------------------------------------------
%%% Test functions
%%--------------------------------------------------------------------


% start_test_() ->
% 	{setup, fun() ->
% 		cpx_hooks:start_link(),
% 		spx_agent_auth:start()
% 	end, [
% 		?_assert(has_hook(spx_get_agents, get_agents)),
% 		?_assert(has_hook(spx_get_agents_by_profile, get_agents_by_profile)),
% 		?_assert(has_hook(spx_get_agent, get_agent)),
% 		?_assert(has_hook(spx_auth_agent, auth_agent)),
% 		?_assert(has_hook(spx_get_profiles, get_profiles)),
% 		?_assert(has_hook(spx_get_profile, get_profile))
% 	]}.

% defaults_test_() ->
% 	{setup, fun() ->
% 		meck:new(mongoapi),
% 		meck:expect(mongoapi, new, 2, {mongoapi, spx, <<"imdb_test">>}),
% 		meck:expect(mongoapi, findOne, 3, not_connected),
% 		meck:expect(mongoapi, find, 6, not_connected)
% 	end,
% 	fun(_) -> meck:unload(mongoapi) end,
% 	[?_assertEqual({ok, []}, spx_agent_auth:get_agents()),
% 	?_assertEqual({ok, []}, spx_agent_auth:get_agents_by_profile("prof")),
% 	?_assertEqual(none, spx_agent_auth:get_agent(login, "login")),
% 	?_assertEqual(none, spx_agent_auth:get_agent(id, "id")),
% 	?_assertEqual(pass, spx_agent_auth:auth_agent("u", "p")),
% 	?_assertEqual({ok, []}, spx_agent_auth:get_profiles()),
% 	?_assertEqual(undefined, spx_agent_auth:get_profile("prof")),
% 	?_assertEqual({ok, []}, spx_agent_auth:get_releases())
% 	]}.

% integ_get_agents_test_() ->
% 	{setup, fun reset_test_db/0, fun stop_test_db/1,
% 		[?_assertMatch({ok, [
% 			#agent_auth{id="agent1", login="foo", security_level=admin},
% 			#agent_auth{id="agent2", login="bar", security_level=agent},
% 			#agent_auth{id="agent3", login="baz", security_level=supervisor}
% 		]}, spx_agent_auth:get_agents())]
% 	}.

% integ_get_agents_by_profile_test_() ->
% 	{setup, fun reset_test_db/0, fun stop_test_db/1,
% 		[?_assertMatch({ok, []}, spx_agent_auth:get_agents_by_profile("nada")),
% 		?_assertMatch({ok, [
% 			#agent_auth{id="agent1", login="foo", security_level=admin},
% 			#agent_auth{id="agent3", login="baz", security_level=supervisor}
% 		]}, spx_agent_auth:get_agents_by_profile("foobaz"))]
% 	}.

% integ_get_agent_test_() ->
% 	{setup, fun reset_test_db/0, fun stop_test_db/1,
% 		[?_assertMatch(none, spx_agent_auth:get_agent(login, "nada")),
% 		?_assertMatch(none, spx_agent_auth:get_agent(id, "noone")),

% 		?_assertMatch({ok,
% 			#agent_auth{id="agent1", login="foo", security_level=admin}},
% 			spx_agent_auth:get_agent(login, "foo")),
% 		?_assertMatch({ok,
% 			#agent_auth{id="agent1", login="foo", security_level=admin}},
% 			spx_agent_auth:get_agent(id, "agent1"))
% 		]
% 	}.

% integ_auth_agent_test_() ->
% 	{setup, fun reset_test_db/0, fun stop_test_db/1,
% 		[?_assertMatch(pass, spx_agent_auth:auth_agent("not", "here")),
% 		?_assertMatch({ok, deny}, spx_agent_auth:auth_agent("foo", "wrongpass")),
% 		?_assertMatch({ok,
% 			{allow, "agent1", _, admin, "foobaz"}}, %% TODO fill up
% 			spx_agent_auth:auth_agent("foo", "foosecret"))
% 		]
% 	}.

% integ_get_profiles_test_() ->
% 	{setup, fun reset_test_db/0, fun stop_test_db/1,
% 		[?_assertMatch({ok, [
% 			#agent_profile{id="group1"},
% 			#agent_profile{id="group2"}]},
% 			spx_agent_auth:get_profiles())
% 		]
% 	}.

% integ_get_profile_test_() ->
% 	{setup, fun reset_test_db/0, fun stop_test_db/1,
% 		[?_assertMatch(undefined, spx_agent_auth:get_profile("noprofile")),
% 		?_assertMatch({ok, #agent_profile{id="group2"}},
% 			spx_agent_auth:get_profile("foobaz"))]
% 	}.

% integ_get_releases_test_() ->
% 	{setup, fun reset_test_db/0, fun stop_test_db/1,
% 		[?_assertMatch({ok, [
% 			#release_opt{id="opt1", label="in a meeting", bias= -1},
% 			#release_opt{id="opt2", label="busy", bias=0}
% 			]}, spx_agent_auth:get_releases())]
% 	}.

% %% Test helpers

% has_hook(Name, Hook) ->
% 	lists:member({Name, ?MODULE, Hook, [], 200},
% 		cpx_hooks:get_hooks(Hook)).

% reset_test_db() ->
% 	PrivDir = case code:priv_dir(sipxplugin) of
% 		{error, _} ->
% 			filename:join([filename:dirname(code:which(spx_agent_auth)),
% 				"..", "priv"]);
% 		Dir -> Dir
% 	end,
% 	Path = filename:join(PrivDir, "test_entries.json"),

% 	{ok, Bin} = file:read_file(Path),
% 	{[{<<"entries">>, Entries}]} = ejrpc2_json:decode(Bin),

% 	% mongodb:start(),
% 	mongodb:singleServer(spx),
% 	mongodb:connect(spx),

% 	DB = mongoapi:new(spx,?DB),
% 	DB:set_encode_style(default),

% 	DB:dropDatabase(),
% 	lists:foreach(fun({struct, Props}) ->
% 		Id = proplists:get_value("_id", Props),
% 		P1 = proplists:delete("_id", Props),
% 		P2 = [{<<"_id">>, Id}| P1],
% 		DB:save("entity", P2) end,
% 	Entries).


% stop_test_db(_) ->
% 	% catch mongodb:stop(),
% 	ok.

-endif.
