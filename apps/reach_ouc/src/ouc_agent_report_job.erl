-module(ouc_agent_report_job).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("reach_core/include/agent.hrl").

-define(RSTAT_VSN, 2).
-define(VSN, 1).

%% API
-export([
	start/0,
	start_link/0,
	stop/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-import(cpx_json_util, [l2b/1, b2l/1]).

-define(SERVER, ?MODULE).

-record(st, {
	coverage, buffer, last_start
}).

%% API

start() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:call(?SERVER, stop).

%% gen_server callbacks

init(Opts) ->
	Now = ouc_time:now(),

	Coverage = proplists:get_value(coverage, Opts, 300), % 5 minutes
	Buffer = proplists:get_value(buffer, Opts, 60), % 1 minute

	LastStart = get_initial_last_start(Now, Coverage, Buffer),

	ensureIndexes(),

    {ok, #st{
    	coverage = Coverage,
    	buffer = Buffer,
    	last_start = LastStart
    }, 0}.

handle_call(stop, _From, St) ->
	{stop, normal, ok, St};

handle_call(find_last_start, _From, St) ->
	Reply = find_last_start(900),
	{reply, Reply, St};

handle_call(_Request, _From, St) ->
    Reply = ok,
    {reply, Reply, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(timeout, St) ->
	{noreply, run_jobs(St)};

handle_info(tick, St) ->
	{noreply, run_jobs(St)};

handle_info(_Info, St) ->
    {noreply, St}.

terminate(Reason, _State) ->
	lager:info("Terminating due to ~p", [Reason]),
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%% internal

run_jobs(St) ->
	Now = ouc_time:now(),

	Coverage = St#st.coverage,
	Buffer = St#st.buffer,

	InitStart = St#st.last_start + Coverage,

	LastStart = run_jobs(Now, InitStart, Coverage, Buffer),

	schedule_next_tick(Now, LastStart, Coverage, Buffer),

	St#st{last_start=LastStart}.

run_jobs(Now, LastStart, Coverage, Buffer) ->
	StartNow = get_start_now(Now, Coverage, Buffer),

	case StartNow - LastStart of
		0 ->
			do_mapreduce(LastStart, Coverage),
			add_agent_info(),
			LastStart;
		X when X > 0 ->
			do_mapreduce(LastStart, X, Coverage),
			add_agent_info(),
			StartNow;
		_ ->
			StartNow
	end.

do_mapreduce(StartTs, Coverage) ->
	% Range = StartTs + Coverage,
	do_mapreduce(StartTs, 0, Coverage).

do_mapreduce(StartTs, Range, Coverage) ->
	lager:info("Running agent report mapreduce job for ~s",
		[ouc_time:to_sstring(StartTs)]),

	EndTs = StartTs + Range + Coverage,

	StartDate = ouc_time:unixts_to_erlts(StartTs),
	EndDate = ouc_time:unixts_to_erlts(EndTs),

	lager:info("StartDate : ~p, Coverage ~p, Range ~p",[StartTs, Coverage, Range]),

	MapF = iolist_to_binary([<<"function() {
		var agent = this.ndxs.agent;
		var statKey = this.type.split(\":\")[2];
		var val = this.val;
		var durationKeys = ['released_duration', 'idle_duration',
			'ringing_duration', 'oncall_duration', 'wrapup_duration',
			'call_count', 'call_duration'];

		var agentVals = {};

		// initialize
		for (var i = durationKeys.length - 1; i >= 0; i--) {
		    var key = durationKeys[i];
		    agentVals[key] = 0;
		}

        if (statKey != null) {
            agentVals[statKey] = val
        }

		var cov = ">>, integer_to_list(Coverage), <<";
		var startTs = ">>, integer_to_list(StartTs*1000), <<";
		var adjustedStart = this.from - (this.from % (cov * 1000));
		var start = new Date(adjustedStart);

        emit({agent: agent, start: start, cov: cov, vsn: ">>, integer_to_list(?VSN) ,<<"},
            agentVals);
	}">>]),

	ReduceF = <<"function(agent, vals) {
		  var durationKeys = ['released_duration', 'idle_duration',
		    'ringing_duration', 'oncall_duration', 'wrapup_duration',
		    'call_count', 'call_duration'];

		  var agentVals = {};

		for (var i = durationKeys.length - 1; i >= 0; i--) {
		  var key = durationKeys[i];
		  agentVals[key] = 0;
		};

		for (var j = vals.length - 1; j >= 0; j--) {
		  var val = vals[j];
		  for (var k = durationKeys.length - 1; k >= 0; k--) {
		    var key = durationKeys[k];
		    agentVals[key] += val[key];
		  };
		};

		  return agentVals;
		}">>,

	Out = [{<<"merge">>, <<"agent_report">>},
			{<<"db">>, ouc_db:get_db_name(reports)},
			{<<"nonAtomic">>, true}],

	DurationKeys = ["released_duration", "idle_duration",
		"ringing_duration", "oncall_duration", "wrapup_duration"],

	DurationTypes = [l2b("total:split:" ++ K) || K <- DurationKeys],

	CallInfoKeys = ["call_count", "call_duration"],

	CallInfoTypes = [l2b("total:ended:" ++  K) || K <- CallInfoKeys],

	StatTypes = DurationTypes ++ CallInfoTypes,

	Query = [
		{<<"type">>, [{<<"$in">>, {array, StatTypes}}]},
		{<<"from">>, [{<<"$gte">>, StartDate}, {<<"$lt">>, EndDate}]},
		{<<"ndxs.agent">>, [{<<"$type">>, 2}]},
		{<<"cov">>, 5},
		{<<"vsn">>, ?RSTAT_VSN}
	],

	Res = try_catch(fun() -> ouc_db:exec_cmd(reach, [
		{<<"mapReduce">>, <<"rstats">>},
		{<<"map">>, {code, MapF}},
		{<<"reduce">>, {code, ReduceF}},
		{<<"out">>, Out},
		{<<"query">>, Query}
	]) end, error),

	lager:info("agent report mapreduce job result: ~p", [Res]).

get_initial_last_start(Now, Coverage, Buffer) ->
	case find_last_start(Coverage) of
		{ok, LastStart} ->
			case (LastStart rem Coverage) of
				0 ->
					LastStart;
				Other ->
					lager:warning("Last log date (~p) is not valid for coverage. Rounding off", [LastStart]),
					LastStart - Other
			end;
		_ ->
			Reps = 100,
			StartNow = get_start_now(Now, Coverage, Buffer),
			EffStart = StartNow - (Reps * Coverage) ,

			lager:info("Did not find previous records, starting from ~s",
				[ouc_time:to_sstring(EffStart)]),
			EffStart
	end.

find_last_start(Coverage) ->
	% db.runCommand({aggregate: 'agent_report', pipeline:
	% [{$match: {'coverage': 900, 'vsn': 1}},
	% {$group: {_id: '', lastStart: {$max: '$start'}}}]})

	Res = try_catch(fun() -> ouc_db:exec_cmd(reports, [
		{<<"aggregate">>, <<"agent_report">>},
		{<<"pipeline">>, {array, [
			[{<<"$match">>, [{<<"_id.cov">>, Coverage}, {<<"_id.vsn">>, 1}]}],
			[{<<"$group">>, [{<<"_id">>, <<>>}, {<<"lastStart">>,
				[{<<"$max">>, <<"$_id.start">>}]
			}]}]
		]}}
	]) end, []),

	case lists:keyfind(<<"result">>, 1, Res) of
		{_, {array, [Entry]}} ->
			{_, LastStartErl} = lists:keyfind(<<"lastStart">>, 1, Entry),
			LastStart = ouc_time:erlnow_to_unixts(LastStartErl),

			{ok, LastStart};
		_ ->
			undefined
	end.


get_start_now(Now, Coverage, Buffer) ->
	X = Now - Coverage - Buffer,
	X - (X rem Coverage).

schedule_next_tick(Now, LastStart, Coverage, Buffer) ->
	DelayMs = (Now - (LastStart + Coverage + Buffer)) * 1000,
	erlang:send_after(DelayMs, self(), tick).

add_agent_info() ->
	Db = ouc_db:get_db(reports),
	{ok, Entries} = try_catch(fun() -> Db:find(<<"agent_report">>, [{<<"info">>, null}],
		[{<<"_id">>, 1}], 0, 0) end, {ok, []}),

	Exts = [proplists:get_value(<<"agent">>, Ps) ||
		[{<<"_id">>, Ps}] <- Entries],

	lists:foreach(fun update_agent_name/1, Exts).

update_agent_name(Agent) ->
	{FirstName, LastName, Profile} = get_agent_info(Agent),

	try_catch(fun() -> ouc_db:exec_cmd(reports, [
		{<<"findAndModify">>, <<"agent_report">>},
		{<<"query">>, [
			{<<"info">>, null},
			{<<"_id.agent">>, Agent},
			{<<"_id.vsn">>, ?VSN}
		]},
		{<<"update">>, [
			{<<"$set">>, [
				{<<"info">>, [
					{<<"first_name">>, cpx_json_util:nob(FirstName)},
					{<<"last_name">>, cpx_json_util:nob(LastName)},
					{<<"profile">>, cpx_json_util:nob(Profile)}
				]}
			]}
		]}
	]) end, error).

get_agent_info(Agent) ->
	case agent_auth:get_agent_by_login(binary_to_list(Agent)) of
		{ok, #agent_auth{firstname = FirstName, lastname = LastName, profile = Profile}} ->
			{FirstName, LastName, Profile};
		_ ->
			{null, null, null}
	end.

ensureIndexes() ->

	Db = ouc_db:get_db(reports),
	ensureIndexes(Db, get_indexes()).

ensureIndexes(Db, [{Coll, Index}|Others]) ->

	try Db:ensureIndex(Coll, Index) of
		Res -> Res
	catch
		_Error:Reason -> lager:error("Error creating index ~p for Coll ~p with reason ~p", [Index, Coll, Reason])
	end,
	ensureIndexes(Db, Others);

ensureIndexes(_Db, []) ->
	ok.

get_indexes() ->
	[
		{<<"agent_report">>, [{<<"_id.start">>,1}]}
	].

try_catch(Func, ErrorReturn) ->
	try Func() of
		Result -> Result
	catch
		Error:Reason -> lager:error("caught error ~p with reason ~p on agent report job", [Error, Reason]),
		ErrorReturn
	end.
%% tests

-ifdef(TEST).
-endif.
