-module(ouc_queues).

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("oacd_ouc.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("reach_core/include/call.hrl").
-include_lib("reach_core/include/queue.hrl").
-include_lib("reach_core/include/gen_media.hrl").

-define(SERVER, {local, ?MODULE}).
-define(SUBSCRIBE_KEY, {p, g, ouc_queue_count_update}).

-ifndef(TEST).
-define(CHECK_INTERVAL, 1000).
-else.
-define(CHECK_INTERVAL, 100).
-endif.

%% API
-export([start/0, start_link/0, stop/0,
	get_queue_counts/0, get_queue_counts/1, subscribe/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {queue_counts, check_flag=false, check_timer}).

%% API
-spec start() -> {ok, pid()} | ignore | {error, any()}.
start() ->
	gen_server:start(?SERVER, ?MODULE, [], []).

-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
	gen_server:start_link(?SERVER, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
	gen_server:call(?MODULE, stop).

-spec get_queue_counts() -> [#queue_count{}].
get_queue_counts() ->
	get_queue_counts([]).

-spec get_queue_counts(list()) -> [#queue_count{}].
get_queue_counts(Opts) ->
	gen_server:call(?MODULE, {get_queue_counts, Opts}).

-spec subscribe() -> ok.
subscribe() ->
	catch gproc:reg(?SUBSCRIBE_KEY),
	ok.

%% gen_server callbacks
init([]) ->
	gproc:add_global_property(cpx_media_change, subscribe),
	gproc:add_global_property(cpx_queue_config_change, subscribe),
	Queues = compute_queue_counts(),
	TimerRef = start_check_timer(),
	{ok, #state{queue_counts=Queues, check_timer=TimerRef}}.

handle_call({get_queue_counts, _Opts}, _From, State) ->
	Queues = compute_queue_counts(),
	{reply, Queues, State#state{queue_counts = Queues}};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
	{reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({cpx_queue_config_change, {loaded_queue, _}}, State) ->
	{noreply, State#state{check_flag=true}};
handle_info({cpx_queue_config_change, {terminated_queue, _}}, State) ->
	{noreply, State#state{check_flag=true}};
handle_info(#cpx_gen_media_init{}, State) ->
	{noreply, State#state{check_flag=true}};
handle_info(#cpx_gen_media_update{}, State) ->
	{noreply, State#state{check_flag=true}};
handle_info({timeout, Timer, check}, State = #state{check_timer=Timer}) ->
	Timer2 = start_check_timer(),
	Queues = case State#state.check_flag of
		true ->
			OQueues = State#state.queue_counts,
			NQueues = compute_queue_counts(),

			Diffs = (catch get_count_diffs(OQueues, NQueues, [])),
			send_count_diffs(Diffs),

			NQueues;
		_ ->
			State#state.queue_counts
	end,
	State1 = State#state{queue_counts=Queues, check_timer=Timer2, check_flag=false},
	{noreply, State1};
handle_info(_Msg, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

%% Internal

start_check_timer() ->
	erlang:start_timer(?CHECK_INTERVAL, self(), check).

-type count_diff() :: #queue_count{} | {queue_delete, Queue::string()}.
-spec get_count_diffs(Old::[#queue_count{}], New::[#queue_count{}], [count_diff()]) -> [count_diff()].
get_count_diffs([], [], Acc) ->
	lists:reverse(Acc);
get_count_diffs([H|T1], [H|T2], Acc) ->
	get_count_diffs(T1, T2, Acc);
get_count_diffs([#queue_count{queue=P}|T1], [#queue_count{queue=P}=H2|T2], Acc) ->
	get_count_diffs(T1, T2, [H2|Acc]);

% %% Deleted old queue
get_count_diffs([#queue_count{queue=Q1}|T1], [#queue_count{queue=Q2}|_]=L2, Acc) when Q1 < Q2 ->
	get_count_diffs(T1, L2, [{queue_delete, Q1}|Acc]);
get_count_diffs([#queue_count{queue=Q1}|T1], [], Acc) ->
	get_count_diffs(T1, [], [{queue_delete, Q1}|Acc]);

%% New queue
get_count_diffs([#queue_count{queue=Q1}|_]=L1, [#queue_count{queue=Q2}=H2|T2], Acc) when Q1 > Q2 ->
	get_count_diffs(L1, T2, [H2|Acc]);
get_count_diffs([], [H2|T2], Acc) ->
	get_count_diffs([], T2, [H2|Acc]).

compute_queue_counts() ->
	{ok, Queues} = call_queue_config:get_queues(),
	QueuedCalls = get_queued_calls(),
	ConnectedCalls = get_connected_calls(),
	GetInQueue = fun(Name) -> length(proplists:get_all_values(Name, QueuedCalls)) end,
	GetConnected = fun(Name) -> length(proplists:get_all_values(Name, ConnectedCalls)) end,
	lists:sort([#queue_count{queue = Name, calls_queued = GetInQueue(Name), calls_connected = GetConnected(Name)} || #call_queue{name=Name} <- Queues]).

get_queued_calls() ->
	qlc:e(qlc:q([{Q, S} ||
		{_, _, #cpx_gen_media_prop{state=S, queue=Q}} <- gproc:table({g, p}), lists:member(S, [inqueue, inqueue_ringing, ringing])])).

get_connected_calls() ->
	qlc:e(qlc:q([{Q, S} ||
		{_, _, #cpx_gen_media_prop{state=S, queue=Q}} <- gproc:table({g, p}), S =:= oncall])).

send_count_diffs([]) ->
	ok;
send_count_diffs(Diffs) ->
	gproc:send({p, g, ouc_queue_count_update},
		{ouc_queue_count, Diffs}).

-ifdef(TEST).

t_media(Queue, State) ->
	t_media(Queue, State, false).

t_media(Queue, State, Notify) ->
	spawn_link(fun() ->
		Prop = #cpx_gen_media_prop{queue = Queue, state = State},
		gproc:add_global_property(cpx_media, Prop),
		case Notify of
			true ->
				Event = #cpx_gen_media_init{},
				gproc:send({p, g, cpx_media_change}, Event);
			_ ->
				ok
		end,
		receive kill -> ok end
	end).

get_queue_counts_test_() ->
	{spawn, {setup, fun() ->
		application:start(gproc),
		call_queue_config:start(),
		call_queue_config_ets:load_queues(["sales", "marketing"]),

		start()
	end, fun(_) ->
		stop()
	end, [{"get queues", fun() ->
		gproc:reg({p, g, ouc_queue_count_update}),
		M1 = t_media("sales", inqueue),
		M2 = t_media("sales", inqueue_ringing),

		?assertEqual(lists:sort([#queue_count{queue = "sales", calls_queued = 2},
			#queue_count{queue = "marketing", calls_queued = 0, calls_connected=0}]), get_queue_counts()),

		M3 = t_media("sales", ringing, true),
		?assertEqual({ouc_queue_count, [#queue_count{queue="sales", calls_queued=3, calls_connected=0}]},
			receive {ouc_queue_count, _} = Diff -> Diff after 110 -> diff_not_received end),

		M1 ! kill,
		M2 ! kill,
		M3 ! kill
	end}]}}.

-endif.
