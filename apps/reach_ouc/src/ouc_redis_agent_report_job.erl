%% ===========================================================================
%% @copyright 2014 eZuce Inc.
%% @author Costin-Tiberiu Radu
%% @doc Agent Report generator job - redis rstats
%% @end
%% ===========================================================================

-module(ouc_redis_agent_report_job).
-behaviour(gen_server).


%% API
-export([start_link/0,
        stop/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% internal ...
-export([get_agents/0,
         get_agentgroups/0,
         update_job_info/2
         ]).

%% mongo functions - to delete in future
%% -export([find_mongo_last_start/0]).


-record(state, {
    coverage,
    buffer,
    last_start,
    last_duration,
    tref
}).

-define(LAST_RUN_REDIS,<<"reports:agent:last_run">>).
-define(LAST_DURATION_REDIS, <<"reports:agent:last_duration">>).

%% Default coverage of an agent report generation
-define(DEFAULT_COVERAGE, 300).
%% Default buffer - time execution should not exceed this.
-define(DEFAULT_BUFFER, 60).

-include_lib("erlmongo/include/erlmongo.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
% @end
%%--------------------------------------------------------------------
init(_State) ->
    %% Now = ouc_time:now(),
    %% @todo tune the coverage and buffer values
    %%
    ReportsOpts = ouc_config:get_options(reach_ouc, reports),
    AgentReportsOpts = ouc_config:get_property(agent, ReportsOpts, {coverage, 300}),
    %% All values are specified in seconds
    Coverage = ouc_config:get_property(coverage, AgentReportsOpts, ?DEFAULT_COVERAGE),
    Buffer = ouc_config:get_property(buffer, AgentReportsOpts, ?DEFAULT_BUFFER),

    ExecInterval = Coverage,
    LastExec = get_last_start(),
    LastStart = proplists:get_value(last_start, LastExec),
    LastDuration = proplists:get_value(last_duration, LastExec),

    lager:info("Agent report genertion job runs every ~p seconds with the buffer ~p",[ExecInterval, Buffer]),
    lager:info("Agent report last execution was at: ~p and lasted ~p",[LastStart, LastDuration]),
    {ok, TRef} = timer:send_interval(ExecInterval * 1000, do_report),

    %%LastStart = get_initial_last_start(Now, Coverage, Buffer),
    State = #state{
        coverage = Coverage,
        buffer = Buffer,
        last_start = LastStart,
        last_duration = LastDuration,
        tref = TRef
    },
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(do_report, State) ->
    Now = ouc_time:now(),
    Coverage = State#state.coverage,
    Buffer   = State#state.buffer,
    LastExec = get_last_start(),
    LastStart = proplists:get_value(last_start, LastExec),

    lager:info("do_report - LastStart: ~p",[LastStart]),
    Duration = do_generate_reports(Now, LastStart, Buffer, Coverage),
    %Duration = ouc_time:now() - Now,
    LastStartTwo = update_job_info(Now, Duration),
    {noreply, State#state{last_start = LastStartTwo}};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    lager:warning("The module is shuting down"),
    Tref = State#state.tref,
    timer:cancel(Tref),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Get last execution time and last duration time,
%% otherwise assume this is the first time and return
%% {last_start, 0}, {last_duration, -1}
%% @end
get_last_start() ->
    RedisDb = ouc_db_master:get_connection(redis),
    % get last_run (and last_duration)
    DbStart = RedisDb:req(read, [<<"get">>, ?LAST_RUN_REDIS], int),
    DbDuration = RedisDb:req(read, [<<"get">>, ?LAST_DURATION_REDIS], int),

    LastStart = DbStart,
    LastDuration = DbDuration,
    [
        {last_start, LastStart},      %% unix timestamp
        {last_duration, LastDuration} %% miliseconds
    ].

%% @todo - deprecated - imported
%%% imported from the old one
%get_initial_last_start(Now, Coverage, Buffer) ->
%	case find_last_start() of
%        {last_start, LastStart} ->
%            lager:info("Agent report last_time -> ~p",[ouc_time:to_sstring(LastStart)]),
%			case (LastStart rem Coverage) of
%				0 ->
%					LastStart;
%				Other ->
%					lager:warning("Last log date (~p) is not valid for coverage. Rounding off", [LastStart]),
%					LastStart - Other
%			end;
%		_ ->
%			Reps = 100,
%			StartNow = get_start_now(Now, Coverage, Buffer),
%			EffStart = StartNow - (Reps * Coverage) ,
%
%			lager:info("Did not find previous records, starting from ~s",
%				[ouc_time:to_sstring(EffStart)]),
%			EffStart
%	end.

%%% imported from the old one
%%get_start_now(Now, Coverage, Buffer) ->
%%	X = Now - Coverage - Buffer,
%%	X - (X rem Coverage).

%% @doc Updates metadata about the last executed job.
%% last_run <unix-timestamp> and
%% last_duration <miliseconds>
%% @end
-spec update_job_info(Now::integer(), Duration::integer()) -> ok.
update_job_info(Now, Duration) ->
    RedisDb = ouc_db_master:get_connection(redis),
    {ok, _Card} = RedisDb:req(write, [<<"set">>, ?LAST_RUN_REDIS, Now]),
    {ok, _Card} = RedisDb:req(write, [<<"set">>, ?LAST_DURATION_REDIS, Duration]),
    ok.

do_generate_reports(Now, StartTs, Range, Coverage) ->
    %% @todo take into account the last start value, not only the precomputed values
    %RedisDb = ouc_db_master:get_connection(redis),
    AgentList = get_agents(),
    AgentGroups = get_agentgroups(),
    _RedisDb = ouc_db_master:get_connection(redis),
    _EndTs = StartTs + Range + Coverage,

    AgentParams = [
        {'released_duration',0},
        {'idle_duration', 0},
        {'ringing_duration', 0},
        {'oncall_duration', 0},
        {'wrapup_duration', 0},
        {'call_count',0},
        {'call_duration'}
    ],

    lager:debug("Agent params ~p ",[AgentParams]),

    %% start = startTs * 1000
    %% adjustedStart = from - (from % (cov * 1000)) %%% from redis keys

    %% Step1: for all keys sum values between (now) and (now-cov)
    %% Step2: group keys by rstat types (total:split_duration, total:call_duration) and
    %%  sum all values from all agents for the same rstat type
    %% should we sum up keys with both agent:.. and agent:...:profile:AgGroup ???
    %AgentValueFun = fun(_Agent) ->
        %% for all duration keys ...
        %KeyName = lists:flatten(["rstat:s:"]),
        %KeyName
    %end,

    %% @todo add mapreduce function here
    SumFun = fun(ElemList) ->
        lists:sum(ElemList)
    end,
    _AgentValues = lists:map(SumFun, AgentList),
    % AgentGroupValues = lists:map(SumFun, AgentGroupList),

    lager:info("Agents: ~p",[AgentList]),
    lager:info("Agent Groups: ~p",[AgentGroups]),
    Duration = ouc_time:now() - Now,
    Duration.


get_agents() ->
    Db = ouc_db:get_db(imdb),
    {ok, AllAgentsEnc} = Db:find(<<"entity">>, #search{criteria = [{<<"ent">>, <<"openacdagent">>}], field_selector = [{<<"name">>, true}]}),
    ParseFun = fun(Elem) ->
        BinName = ej:get({<<"name">>}, Elem, null),
        StrName = binary:bin_to_list(BinName),
        StrName
    end,
    AgentList = lists:map(ParseFun, AllAgentsEnc),
    AgentList.

get_agentgroups() ->
    Db = ouc_db:get_db(imdb),
    {ok, AgGrList } = Db:find(<<"entity">>, #search { criteria = [{<<"ent">>, <<"openacdagentgroup">>}], field_selector = [{<<"name">>, true}]}),
    ParseFun = fun(Elem) ->
        BinName = ej:get({<<"name">>}, Elem, null),
        StrName = binary:bin_to_list(BinName),
        StrName
    end,
    AgentGroupList = lists:map(ParseFun, AgGrList),
    AgentGroupList.
