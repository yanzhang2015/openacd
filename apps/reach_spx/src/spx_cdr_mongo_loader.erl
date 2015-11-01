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

-module(spx_cdr_mongo_loader).

-export([start/0]).

-include_lib("reach_core/include/cpx.hrl").

start() ->
    ActionFun = fun get_action/1,
    LoadFun = ReloadFun = fun load/1,
    UnloadFun = fun unload/1,
    spx_autoloader:add_mod({?MODULE, ActionFun, LoadFun, UnloadFun, ReloadFun}, none).

get_action(none) -> {load, []};
get_action(_) -> none.

load(_) ->
	cpx_supervisor:update_conf(gen_cdr_dumper,
		#cpx_conf{
			id = gen_cdr_dumper,
			module_name = gen_cdr_dumper,
			start_function = start_link,
			start_args = [spx_cdr_mongo, []],
			supervisor = management_sup}).

unload(_) ->
	cpx_supervisor:destroy(gen_cdr_dumper).
