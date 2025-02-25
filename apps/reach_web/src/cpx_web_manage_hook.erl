%% Copyright (c) 2012 eZuce, Inc. All rights reserved.
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

-module(cpx_web_manage_hook).

-export([register_hooks/0, get_cpx_managed/0, handle_web/3]).

-type cpx_managed_opt() :: {web_docroot, string()}.
-type cpx_managed() :: {module(), [cpx_managed_opt()]}.

-include("oacd_web.hrl").
-include_lib("reach_core/include/cpx.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec register_hooks() -> ok.
register_hooks() ->
	cpx_hooks:set_hook(cpx_web_manage_hook, get_cpx_managed,
		{cpx_web_manage_hook, get_cpx_managed, []}),
	ok.

-spec get_cpx_managed() -> [cpx_managed()].
get_cpx_managed() ->
	WebDocRoot = filename:join(code:priv_dir(oacd_web), "admin"),

	OacdWeb = {oacd_web,
		[{web_docroot, WebDocRoot},
		{web_handle, {?MODULE, handle_web}}]},
	{ok, [OacdWeb]}.

handle_web(get, Node, _Post) ->
	Props = case rpc:call(Node, cpx_supervisor, get_conf, [oacd_web]) of
		undefined ->
			[{success, true}, {enabled, false}];
		Rec when is_record(Rec, cpx_conf) ->
			[Opts] = Rec#cpx_conf.start_args,
			Port = proplists:get_value(port, Opts, ?DEFAULT_PORT),
			[{success, true}, {enabled, true}, {port, Port}]
	end,

	{200, [], ejrpc2_json:encode({struct, Props})};
handle_web(update, Node, Post) ->
	case proplists:get_value("enabled", Post) of
		"true" ->
			Port = case catch list_to_integer(proplists:get_value("port", Post)) of
				N when is_integer(N) ->
					N;
				_ ->
					?DEFAULT_PORT
			end,
			Conf = #cpx_conf{
				id = oacd_web,
				module_name = cpx_agent_web_listener,
				start_function = start_link,
				start_args = [[{port, Port}]],
				supervisor = agent_connection_sup
			},
			rpc:call(Node, cpx_supervisor, update_conf, [oacd_web, Conf]);
		_ ->
			rpc:call(Node, cpx_supervisor, destroy, [oacd_web])
	end,
	{200, [], ejrpc2_json:encode({struct, [{success, true}]})}.

%% Tests
-ifdef(TEST).

register_hook_test_() ->
	{setup, fun() ->
		meck:new(cpx_hooks),
		meck:expect(cpx_hooks, set_hook, 3, ok)
	end, fun(_) ->
		meck:unload(cpx_hooks)
	end, [fun() ->
		cpx_web_manage_hook:register_hooks(),
		?assert(meck:called(cpx_hooks, set_hook, [cpx_web_manage_hook, get_cpx_managed,
	 		{cpx_web_manage_hook, get_cpx_managed, []}], self()))
	end]}.

% get_cpx_managed_test() ->
% 	?assertEqual_(
% 		{ok, [{oacd_web,
% 			[{web_docroot, code:priv_dir(oacd_web) ++ "/config"},
% 			{web_handle, {cpx_web_manage_hook, handle_web}}]}]},
% 		cpx_web_manage_hook:get_cpx_managed()).

handle_web_test_() ->
	{setup, fun() ->
		meck:new(cpx_supervisor)
	end, fun(_) ->
		meck:unload(cpx_supervisor)
	end, [{"get - no config", fun() ->
		meck:expect(cpx_supervisor, get_conf, fun(oacd_web) -> undefined end),
		{200, [], Resp} = cpx_web_manage_hook:handle_web(get, node(), []) ,
		{struct, Props} = ejrpc2_json:decode(iolist_to_binary(Resp)),
		?assertEqual([{<<"success">>, true}, {<<"enabled">>, false}], Props)
	end}, {"get - with port", fun() ->
		Opts = [{port, 5123}],
		Conf = #cpx_conf{id=oacd_web, module_name=cpx_agent_web_listener, start_function=start_link, start_args=[Opts]},
		meck:expect(cpx_supervisor, get_conf, fun(oacd_web) -> Conf end),
		{200, [], Resp} = cpx_web_manage_hook:handle_web(get, node(), []) ,
		{struct, Props} = ejrpc2_json:decode(iolist_to_binary(Resp)),
		?assertEqual([{<<"success">>, true}, {<<"enabled">>, true}, {<<"port">>, 5123}], Props)
	end}, {"get - without port", fun() ->
		Opts = [],
		Conf = #cpx_conf{id=oacd_web, module_name=cpx_agent_web_listener, start_function=start_link, start_args=[Opts]},
		meck:expect(cpx_supervisor, get_conf, fun(oacd_web) -> Conf end),
		{200, [], Resp} = cpx_web_manage_hook:handle_web(get, node(), []) ,
		{struct, Props} = ejrpc2_json:decode(iolist_to_binary(Resp)),
		?assertEqual([{<<"success">>, true}, {<<"enabled">>, true}, {<<"port">>, ?DEFAULT_PORT}], Props)
	end}, {"update - disable", fun() ->
		meck:expect(cpx_supervisor, destroy, 1, ok),
		{200, [], Resp} = cpx_web_manage_hook:handle_web(update, node(), [{"enabled", "false"}]),
		{struct, Props} = ejrpc2_json:decode(iolist_to_binary(Resp)),
		?assertEqual([{<<"success">>, true}], Props),
		?assert(meck:called(cpx_supervisor, destroy, [oacd_web], self()))
	end}, {"update - enable with port", fun() ->
		Conf = #cpx_conf{
			id = oacd_web,
			module_name = cpx_agent_web_listener,
			start_function = start_link,
			start_args = [[{port, 5123}]],
			supervisor = agent_connection_sup
		},
		meck:expect(cpx_supervisor, update_conf, 2, ok),
		{200, [], Resp} = cpx_web_manage_hook:handle_web(update, node(), [{"enabled", "true"}, {"port", "5123"}]),
		{struct, Props} = ejrpc2_json:decode(iolist_to_binary(Resp)),
		?assertEqual([{<<"success">>, true}], Props),
		?assert(meck:called(cpx_supervisor, update_conf, [oacd_web, Conf], self()))
	end}]}.


-endif.
