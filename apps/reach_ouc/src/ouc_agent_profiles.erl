-module(ouc_agent_profiles).

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("oacd_ouc.hrl").
-include("ouc_rstat.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("reach_core/include/agent.hrl").

-define(SERVER, {local, ?MODULE}).
-define(SUBSCRIBE_KEY, {p, g, ouc_agent_profile_stats_update}).

-ifndef(TEST).
-define(CHECK_INTERVAL, 1000).
-else.
-define(CHECK_INTERVAL, 100).
-endif.

-define(PROFILE_STATE_PROP(Profile, State), {ouc_profile_state, Profile, State}).

%% API
-export([start/0, start_link/0, stop/0,
	get_profiles/0, get_profiles/1,
	update_profile_counts/3,

	subscribe/0, unsubscribe/0, get_subscribers/0, is_subscribed/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {stats, check_flag=false, check_timer}).

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

-spec get_profiles() -> [#profile_counts{}].
get_profiles() ->
	get_profiles([]).

-spec get_profiles([{force, boolean()}]) -> [#profile_counts{}].
get_profiles(Opts) ->
	gen_server:call(?MODULE, {get_profiles, Opts}).

%% @doc subscribes to agent profile stats changes
-spec subscribe() -> ok.
subscribe() ->
	catch gproc:reg(?SUBSCRIBE_KEY),
	ok.

-spec unsubscribe() -> ok.
unsubscribe() ->
	gproc:unreg(?SUBSCRIBE_KEY),
	ok.

-spec get_subscribers() -> ok.
get_subscribers() ->
	gproc:lookup_pids(?SUBSCRIBE_KEY).

-spec is_subscribed() -> ok.
is_subscribed() ->
	lists:member(self(), get_subscribers()).

-spec update_profile_counts(Profile::string(),
	LastUState::atom(), UState::atom()) -> boolean().
update_profile_counts(_Profile, UState, UState) ->
	false;
update_profile_counts(Profile, undefined, UState) ->
	gproc:mreg(p, g, [{?PROFILE_STATE_PROP(Profile, UState), now()},
		{ouc_ustate, UState}]);
update_profile_counts(Profile, LastUState, undefined) ->
	gproc:unreg({p, g, ?PROFILE_STATE_PROP(Profile, LastUState)});
update_profile_counts(Profile, LastUState, UState) ->
	gproc:unreg({p, g, ?PROFILE_STATE_PROP(Profile, LastUState)}),
	gproc:reg({p, g, ?PROFILE_STATE_PROP(Profile, UState)}, now()),
	gproc:set_value({p, g, ouc_ustate}, UState).

%% gen_server callbacks
init([]) ->
	gproc:add_global_property(cpx_agent_change, subscribe),
	gproc:add_global_property(cpx_agent_channel_change, subscribe),
	Stats = get_counts(),
	TimerRef = start_check_timer(),
	{ok, #state{stats=Stats, check_timer=TimerRef}}.

handle_call({get_profiles, Opts}, _From, State) ->
	case proplists:get_value(force, Opts) of
		true ->
			Stats = get_counts(),
			{reply, Stats, State#state{stats = Stats}};
		_ ->
			Stats = State#state.stats,
			{reply, Stats, State}
	end;
handle_call(stop, _From, State) ->
	lager:info("Stopping ouc_agent_profiles...", []),
	{stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
	lager:warning("Unknown call from ~p: ~p", [_From, _Msg]),
	{reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
	lager:warning("Unknown cast: ~p", [_Msg]),
	{noreply, State}.

handle_info({{_, _}, #cpx_agent_prop{}}, State) ->
	{noreply, State#state{check_flag=true}};
handle_info({{_, _}, #cpx_agent_channel_prop{}}, State) ->
	{noreply, State#state{check_flag=true}};
handle_info(#cpx_agent_login{pid = Pid}, State) ->
	erlang:monitor(process, Pid),
	{noreply, State#state{check_flag=true}};
handle_info(#cpx_agent_logout{pid = _Pid}, State) ->
	{noreply, State#state{check_flag = true}};
handle_info(#cpx_agent_state_update{}, State) ->
	{noreply, State#state{check_flag=true}};
handle_info(#cpx_agent_channel_state_update{}, State) ->
	{noreply, State#state{check_flag=true}};
handle_info({'DOWN', _, process, _, _}, State) ->
	{noreply, State#state{check_flag=true}};
handle_info({timeout, Timer, check}, State = #state{check_timer=Timer}) ->
	Timer2 = start_check_timer(),
	OStats = State#state.stats,

	NStats = case State#state.check_flag of
		true ->
			NStats1 = get_counts(),

			Diffs = get_count_diffs(OStats, NStats1, []),
			send_count_diffs(Diffs),

			NStats1;
		_ ->
			OStats
	end,

	NState = State#state{stats=NStats, check_timer=Timer2, check_flag=false},
	{noreply, NState};
handle_info(_Msg, State) ->
	lager:warning("Unknown info: ~p", [_Msg]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

%% Internal
start_check_timer() ->
	erlang:start_timer(?CHECK_INTERVAL, self(), check).

get_counts() ->
	Profiles = get_profile_names(),

	% Assume everyone's idle first if available
	% LoginEntries = [{Login, {Profile, State =:= available, idle}} ||
	% 	{_, #cpx_agent_prop{login=Login, profile=Profile, state=State}} <- gproc:lookup_local_properties(cpx_agent)],

	% % Get the channels - {agent, chanstate}
	% ChanEntries = [{Login, ChanState} ||
	% 	{_, #cpx_agent_channel_prop{login=Login, state=ChanState}} <- gproc:lookup_local_properties(cpx_agent_channel)],

	% % Update the agents with new states based on channel if there's any
	% AgentDict = update_entries(ChanEntries, dict:from_list(LoginEntries)),

	% % Initialize count
	% InitProfiles = dict:from_list([
	% 	{Profile, #profile_counts{}} || Profile <- Profiles]),

	% % Update the count based on agent/channel state
	% Counts = dict:fold(fun(_, {Profile, IsAvailable, CState}, Acc) ->
	% 	dict:update(Profile, fun(Counts) ->
	% 		update_counts(Counts, IsAvailable, CState)
	% 	end, Acc)
	% end, InitProfiles, AgentDict),

	% lists:keysort(1, dict:to_list(Counts)).

	lists:keysort(1, [{Profile, get_profile_count(Profile)} || Profile <- Profiles]).

get_profile_count(Profile) ->
	#profile_counts{
		total = get_gproc_count(Profile),
		released = get_gproc_count(Profile, released),
		idle = get_gproc_count(Profile, idle),
		ringing = get_gproc_count(Profile, ringing),
		oncall = get_gproc_count(Profile, oncall),
		wrapup = get_gproc_count(Profile, wrapup)
	}.

get_gproc_count(Profile) ->
	length(gproc:lookup_pids({p, g, {cpx_profile, Profile}})).
get_gproc_count(Profile, State) ->
	length(gproc:lookup_pids({p, g, {ouc_profile_state, Profile, State}})).

get_profile_names() ->
	{ok, Profiles} = agent_auth:get_profiles(),
	lists:sort([Name || #agent_profile{name=Name} <- Profiles]).

% update_counts(Counts, IsAvailable, CState) ->
% 	Y = case CState of
% 			ringing -> #profile_counts.ringing;
% 			oncall -> #profile_counts.oncall;
% 			wrapup -> #profile_counts.wrapup;
% 			_ -> #profile_counts.idle
% 	end,
% 	{R, A} = element(Y, Counts),
% 	NCount = case IsAvailable of
% 		true -> {R, A+1};
% 		_ -> {R+1, A}
% 	end,

% 	setelement(Y, Counts, NCount).

% update_entries([], Acc) ->
% 	Acc;
% update_entries([{Login, ChanState}|T], Acc) ->
% 	Acc1 = dict:update(Login, fun({Profile, IsAvailable, _}) ->
% 		%% TODO use this to handle multi channel state
% 		{Profile, IsAvailable, ChanState}
% 	end, Acc),
% 	update_entries(T, Acc1).

send_count_diffs([]) ->
	ok;
send_count_diffs(Diffs) ->
	gproc:send({p, g, ouc_agent_profile_stats_update},
		{ouc_agent_profile_count, Diffs}).

% send_rstats_diff([]) ->
% 	ok;
% send_rstats_diff(Diffs) ->
% 	gproc:send({p, g, ouc_agent_profile_stats_update},
% 		{ouc_agent_profile_rstats, Diffs}).

-type count_diff() :: {profile_update, Profile::string(), #profile_counts{}} | {profile_delete, Profile::string()}.
-spec get_count_diffs(Old::[#profile_counts{}], New::[#profile_counts{}], [count_diff()]) -> [count_diff()].
get_count_diffs([], [], Acc) ->
	lists:reverse(Acc);
get_count_diffs([H|T1], [H|T2], Acc) ->
	get_count_diffs(T1, T2, Acc);
get_count_diffs([{P, _}|T1], [{P, U2}|T2], Acc) ->
	get_count_diffs(T1, T2, [{profile_update, P, U2}|Acc]);

% %% Deleted old profile
get_count_diffs([{P1, _}|T1], [{P2, _}|_]=L2, Acc) when P1 < P2 ->
	get_count_diffs(T1, L2, [{profile_delete, P1}|Acc]);
get_count_diffs([{P1, _}|T1], [], Acc) ->
	get_count_diffs(T1, [], [{profile_delete, P1}|Acc]);

%% New profile
get_count_diffs([{P1, _}|_]=L1, [{P2, U2}|T2], Acc) when P1 > P2 ->
	get_count_diffs(L1, T2, [{profile_update, P2, U2}|Acc]);
get_count_diffs([], [{P2, U2}|T2], Acc) ->
	get_count_diffs([], T2, [{profile_update, P2, U2}|Acc]).

-ifdef(TEST).

t_mk_login() ->
	{X, Y, Z} = erlang:now(),
	lists:append(["agent", integer_to_list(X),
		integer_to_list(Y), integer_to_list(Z)]).

t_agent(Profile, State, ChanState) ->
	t_agent(Profile, State, ChanState, false).

t_agent(Profile, State, ChanState, Notify) ->
	Login = t_mk_login(),
	St = case State of
		available -> available;
		_ -> {"rel1", "In a meeting", 1}
	end,
	Self = self(),
	AgPid = spawn_link(fun() ->
		Prop = #cpx_agent_prop{login=Login, profile=Profile, state=St},
		gproc:add_global_property(cpx_agent, Prop),
		gproc:add_global_property({cpx_profile, Profile}, now()),
		UState = case {State, ChanState} of
			{available, none} -> idle;
			{_, none} -> released;
			_ -> ChanState
		end,
		update_profile_counts(Profile, undefined, UState),

		case Notify of
			true ->
				gproc:send({p, g, cpx_agent_change},
					#cpx_agent_login{pid = self(), prop = Prop});
			_ ->
				ok
		end,
		Self ! ag_ready,
		receive _ -> ok end
	end),

	t_wait(),

	AgPid.

t_wait() ->
	receive
		ag_ready -> ok
	after 10 ->
		throw(agent_spawn_fail)
	end.

t_expect_profiles(Profiles) ->
	meck:expect(agent_auth, get_profiles, 0,
			{ok, [#agent_profile{name=X, id=X} || X <- Profiles]}).

t_assert_state_counts(Counts) ->
	?assertEqual(lists:sort(Counts), lists:sort(get_profiles())).

t_receive_diffs() ->
	receive
		{ouc_agent_profile_count, DiffCounts} -> DiffCounts
	after 200 -> undefined
	end.

t_assert_receive_diffs(Diffs) ->
	Update = t_receive_diffs(),
	?assert(is_list(Update)),
	?assertEqual(lists:sort(Diffs), lists:sort(Update)).

get_profiles_test_() ->
	Confs = [{dynamic, last_30m, {minute, 30}, {second, 1}, 0}],

	InitStats = [
		profile_counts("sales"),
		profile_counts("marketing"),
		profile_counts("admin"),
		profile_counts("rnd")
	],

	{foreach, fun() ->
		application:start(gproc),

		%% TODO remove this or make it a mock
		application:set_env(reach_ouc, rolling_stats, Confs),

		meck:new(agent_auth),

		t_expect_profiles(["sales", "marketing", "admin", "rnd"]),

		start()
	end, fun(_) ->
		catch stop(),
		gproc:send({p, g, cpx_agent}, exit),
		gproc:send({p, g, cpx_agent_channel}, exit),
		meck:unload()
	end, [{"init", fun() ->
		?assertEqual(
			lists:sort(InitStats),
			lists:sort(get_profiles()))
	end}, {"forced", fun() ->
		t_agent("sales", released, none),
		t_agent("sales", released, wrapup),
		t_agent("sales", available, none),
		t_agent("sales", available, none),
		t_agent("sales", available, ringing),
		t_agent("sales", available, oncall),


		t_agent("marketing", released, none),
		t_agent("marketing", available, wrapup),

		t_agent("rnd", available, none),

		%% Idle, Ringing, OnCall, WrapUp
		Counts = lists:sort([
			profile_counts("sales", {6, 1, 2, 1, 1, 1}),
			profile_counts("marketing", {2, 1, 0, 0, 0, 1}),
			profile_counts("admin"),
			profile_counts("rnd", {1, 0, 1, 0, 0, 0})
		]),
		?assertEqual(Counts,
			lists:sort(get_profiles([{force, true}])))
	end}, {"no notifications", fun() ->
		t_agent("sales", available, none),
		t_assert_state_counts(InitStats)
	end}, {"notify new count", fun() ->
		subscribe(),
		Pid = t_agent("sales", available, none, true),
		t_agent("marketing", released, none, true),
		timer:sleep(110),
		t_assert_state_counts([
			%% Idle, Ringing, OnCall, WrapUp
			{"sales", #profile_counts{total=1,idle=1}},
			{"marketing", #profile_counts{total=1,released=1}},
			profile_counts("admin"),
			profile_counts("rnd")
		]),

		t_assert_receive_diffs([
			{profile_update, "sales", #profile_counts{total=1,idle=1}},
			{profile_update, "marketing", #profile_counts{total=1,released=1}}
		]),

		Pid ! kill,
		t_assert_receive_diffs([
			{profile_update, "sales", #profile_counts{}}])
	end}, {"notify new profile", fun() ->
		subscribe(),
		t_expect_profiles(["sales", "hr", "marketing", "admin", "rnd"]),
		%% TODO profile addition should trigger an event on its own
		t_agent("hr", released, none, true),
		t_assert_receive_diffs([
			{profile_update, "hr", #profile_counts{total=1,released=1}}
		])
	end}, {"notify deleted profile", fun() ->
		subscribe(),
		%% to trigger an event:
		t_expect_profiles(["marketing", "admin", "rnd"]),
		%% TODO profile deletion should trigger an event on its own
		t_agent("marketing", released, none, true),
		t_assert_receive_diffs([
			{profile_delete, "sales"},
			{profile_update, "marketing", #profile_counts{total=1,released=1}}
		])
	end}]}.

%% TODO how to notify new empty profile without other events

profile_counts(Profile) ->
	{Profile, #profile_counts{}}.

profile_counts(Profile, {Total, Released, Idle, Ringing, OnCall, WrapUp}) ->
	{Profile, #profile_counts{
		total=Total,
		released=Released,
		idle=Idle,
		ringing=Ringing,
		oncall=OnCall,
		wrapup=WrapUp
	}}.

-endif.
