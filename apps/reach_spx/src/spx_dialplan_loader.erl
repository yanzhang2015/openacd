-module(spx_dialplan_loader).

-export([start/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("reach_core/include/cpx.hrl").
-define(DIALPLAN_CONFIG_TYPE, <<"openacdagentconfigcommand">>).

-record(spx_dialplan_config, {
	autologout :: undefined | non_neg_integer()
}).

start() ->
	unload(none),

	ActionFun = fun get_action/1,
	LoadFun = fun load/1,
	ReloadFun = fun reload/1,
	UnloadFun = fun unload/1,
	spx_autoloader:add_mod({?MODULE, ActionFun, LoadFun, UnloadFun, ReloadFun}, none).

get_action(OldConf) ->
	UpdateConf = get_db_config(fun(X) -> jprop_to_config(X, #spx_dialplan_config{}) end),
	case UpdateConf of
		{ok, OldConf} ->
			none;
		{ok, none} ->
			{unload, none};
		{ok, Conf} ->
			case OldConf of
				none ->
					{load, Conf};
				_ ->
					{reload, Conf}
			end;
		_ ->
			none
	end.

get_db_config(JPropToConfig) ->
	M = mongoapi:new(spx, <<"imdb">>),
	case M:findOne(<<"entity">>, [{<<"type">>, ?DIALPLAN_CONFIG_TYPE}]) of
		{ok, Prop} ->
			JPropToConfig(Prop)
	end.

jprop_to_config([], Acc) ->
	{ok, Acc};
jprop_to_config([{<<"lstenbl">>, false} | _], _) ->
	{ok, none};
jprop_to_config([{<<"autologout">>, V} | T], Acc) when is_integer(V) ->
	jprop_to_config(T, Acc#spx_dialplan_config{autologout = V * 1000});
jprop_to_config([_ | T], Acc) ->
	jprop_to_config(T, Acc).

reload(#spx_dialplan_config{autologout = Autologout}) ->
	lager:info("Setting dialplan autologout to ~p", [Autologout]),
	oacd_dialplan_listener:set_timeout(Autologout);
reload(_) ->
	ok.

-spec load(#spx_dialplan_config{}) -> ok.
load(Conf) ->
	cpx_supervisor:update_conf(oacd_dialplan_listener,
		#cpx_conf{id = oacd_dialplan_listener,
			module_name = oacd_dialplan_listener,
			start_function = start_link,
			start_args = [[{timeout_ms, Conf#spx_dialplan_config.autologout}]],
			supervisor = agent_connection_sup}).

-spec unload(any()) -> ok.
unload(_) ->
	cpx_supervisor:destroy(oacd_dialplan_listener).

-ifdef(TEST).

-endif.
