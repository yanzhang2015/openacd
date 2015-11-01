-module(cpx_ctl).
-export([process/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("agent.hrl").
-include("queue.hrl").
-include("call.hrl").
-include("gen_media.hrl").

-define(RET_SUCCESS, {ok, 0}).
-define(RET_INVALID_COMMAND, {error, 1}).
-define(UNIX_EPOCH_GSEC, 62167219200).

-define(PRINT(Fmt), io:format(Fmt, [])).
-define(PRINT(Fmt, Data), io:format(Fmt, Data)).

-define(TRY(K, Else), try (K) catch _:_ -> Else end).

-record(ctl_agent, {agent, profile, state, login_time}).
-record(ctl_call, {client, state, caller_id, queue, line, state_changes}).

process(["stop"]) ->
	?PRINT("Stopping openacd~n"),
	init:stop(),
	?RET_SUCCESS;

process(["restart"]) ->
	?PRINT("Restarting openacd~n"),
	init:restart(),
	?RET_SUCCESS;

process(["pid"]) ->
	?PRINT("~s~n", [os:getpid()]),
	?RET_SUCCESS;

process(["status"]) ->
	Uptime = ?TRY(get_uptime(), unknown),
	AgentCount = ?TRY(length(agent_manager:list()), unknown),
	QueueCount = ?TRY(get_queue_count(), unknown),
	Plugins = ?TRY(cpx:plugins_running(), []),


	print_uptime(Uptime),
	?PRINT("Number of queues: ~p~n", [QueueCount]),
	?PRINT("Number of agents logged in: ~p~n", [AgentCount]),
	?PRINT("~nPlugins running:~n"),
	[?PRINT("~p~n", [P]) || {P, running} <- Plugins],
	?RET_SUCCESS;

process(["list-agents"]) ->
	Agents = qlc:e(qlc:q([#ctl_agent{agent=Login, profile=Profile, state=State, login_time=StartTime} ||
		{_, _, #cpx_agent_prop{login=Login, profile=Profile, state=State, start_time=StartTime}} <- gproc:table({g, p})])),
	?PRINT("~-10s ~-15s ~-24s ~s~n", ["Login", "Profile", "Login Time", "State"]),
	lists:foreach(fun(A) ->
		print_agent(A)
	end, Agents),
	?RET_SUCCESS;

process(["list-queues"]) ->
	{ok, Queues} = call_queue_config:get_queues(),
	?PRINT("~-20s ~s~n", ["Queue", "Calls in Queue"]),
	lists:foreach(fun(Queue) ->
		QueueName = Queue#call_queue.name,
		CallsInQueue = get_calls_in_queue(QueueName),
		?PRINT("~-20s ", [QueueName]),
		?PRINT("~B~n", [length(CallsInQueue)])
	end, Queues),
	?RET_SUCCESS;

process(["list-calls"]) ->
	Calls = qlc:e(qlc:q([#ctl_call{client=Client, state=State, caller_id=CallerId, queue=Queue, line=Dnis, state_changes=StateChanges} ||
		{_, _, #cpx_gen_media_prop{state=State, call=#call{callerid=CallerId, dnis=Dnis, queue=Queue}, client=#client{label=Client}, state_changes=StateChanges}} <- gproc:table({g, p})])),
	?PRINT("~-15s ~-20s ~-20s ~-20s ~-10s ~s~n", ["Client", "Caller ID", "State", "Queue", "Line", "Start Time"]),
	lists:foreach(fun(C) ->
		print_call(C)
	end, Calls),
	?RET_SUCCESS;

process(["show-agent", Agent]) ->
	case agent_manager:query_agent(Agent) of
		{true, Pid} ->
			 case gproc:info(Pid, gproc) of
				{gproc, [{{p, g, cpx_agent}, #cpx_agent_prop{} = Prop}]} ->
			 		CtlAgent = #ctl_agent{agent=Prop#cpx_agent_prop.login, profile=Prop#cpx_agent_prop.profile,
			 			state=Prop#cpx_agent_prop.state, login_time=Prop#cpx_agent_prop.start_time},
			 			?PRINT("~-10s ~-15s ~-24s ~s~n", ["Login", "Profile", "Login Time", "State"]),
			 		print_agent(CtlAgent);
			 	_ ->
		 			ignore
			 end;
		_ ->
			ignore
	end,
	?RET_SUCCESS;

process(["show-queue", Queue]) ->
	case get_calls_in_queue(Queue) of
		{error, no_exists} ->
			ignore;
		Calls when length(Calls) > 0 ->
			?PRINT("Calls in queue: ~b~n~n", [length(Calls)]),
			?PRINT("~-40s", ["Call ID"]),
			?PRINT("PID~n"),
			lists:foreach(fun({_, Call}) ->
				?PRINT("~-40s", [Call#queued_call.id]),
				?PRINT("~p~n", [Call#queued_call.media])
			end, Calls);
		_ ->
			?PRINT("Calls in queue: 0~n")
	end,
	?RET_SUCCESS;

process(["trace-agent", _Agent]) ->
	?RET_SUCCESS;

process(["kick-agent", Agent]) ->
	case agent_manager:query_agent(Agent) of
		{true, Pid} ->
			?PRINT("Disconnecting agent ~s~n", [Agent]),
			agent:stop(Pid);
		_ ->
			ignore
	end,
	?RET_SUCCESS;

process(_) ->
	print_commands(),
	?RET_INVALID_COMMAND.

get_uptime() ->
	{ok, UpTimestamp} = application:get_env(reach_core, uptime),
	UpTimestamp.

get_queue_count() ->
	{ok, Queues} = call_queue_config:get_queues(),
	length(Queues).

get_calls_in_queue(Name) ->
	case queue_manager:get_queue(Name) of
		undefined ->
			{error, no_exists};
		Pid ->
			call_queue:get_calls(Pid)
	end.

print_uptime(UpTimestamp) when is_integer(UpTimestamp) ->
	Upsince = calendar:gregorian_seconds_to_datetime(UpTimestamp + ?UNIX_EPOCH_GSEC),
	UtcDatetime = calendar:universal_time(),
	Uptime = calendar:time_difference(Upsince, UtcDatetime),
	{UpD, {UpH, UpM, UpS}} = Uptime,
	{{UpsinceYr, UpsinceM, UpsinceD}, {UpsinceH, UpsinceMi, UpsinceS}} = calendar:universal_time_to_local_time(Upsince),

	?PRINT("Uptime: ~b days, ~b hours, ~b minutes, ~b seconds~n", [UpD, UpH, UpM, UpS]),
	?PRINT("Started: ~4..0B/~2..0B/~2..0B ~2..0B:~2..0B:~2..0B~n", [UpsinceYr, UpsinceM, UpsinceD, UpsinceH, UpsinceMi, UpsinceS]);
print_uptime(_) ->
	ok.

print_agent(A) ->
	?PRINT("~-10s ", [A#ctl_agent.agent]),
	?PRINT("~-15s ", [A#ctl_agent.profile]),
	{{Y,M,D}, {H,Mi,S}} = calendar:now_to_local_time(A#ctl_agent.login_time),
	?PRINT("~4..0B/~2..0B/~2..0B ~2..0B:~2..0B:~2..0B      ", [Y,M,D,H,Mi,S]),
	case A#ctl_agent.state of
		available -> ?PRINT("Available~n");
		{released, {_,Reason,_}} -> ?PRINT("Released: ~s~n", [Reason])
	end.

print_call(C) ->
	?PRINT("~-15s ", [C#ctl_call.client]),
	{CallerId1, CallerId2} = C#ctl_call.caller_id,
	?PRINT("~-20s ", [CallerId1 ++ " " ++ CallerId2]),
	?PRINT("~-20s ", [C#ctl_call.state]),
	?PRINT("~-20s ", [C#ctl_call.queue]),
	?PRINT("~-10s ", [C#ctl_call.line]),
	{{Y,M,D}, {H,Mi,S}} = calendar:now_to_local_time(proplists:get_value(init, C#ctl_call.state_changes)),
	?PRINT("~4..0B/~2..0B/~2..0B ~2..0B:~2..0B:~2..0B~n", [Y,M,D,H,Mi,S]).

print_commands() ->
	?PRINT("List of commands~n~n"),
	?PRINT("  stop~n"),
	?PRINT("  restart~n"),
	?PRINT("  pid~n"),
	?PRINT("  status~n"),
	?PRINT("~n"),
	?PRINT("  list-agents~n"),
	?PRINT("  list-queues~n"),
	?PRINT("  list-calls~n"),
	?PRINT("  show-agent [agent]~n"),
	?PRINT("  show-queue [queue]~n"),
	?PRINT("  trace-agent [agent]~n"),
	?PRINT("~n"),
	?PRINT("  kick-agent [agent]~n").
