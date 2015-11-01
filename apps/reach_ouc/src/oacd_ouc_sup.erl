%% ===========================================================================
%% @copyright 2013-2014 eZuce Inc.
%% @author Jan Vincent Liwanag <jvliwanag@ezuce.com>
%% @author Costin-Tiberiu Radu <cradu@ezuce.com>
%% @doc    Reach Ouc main supervisor.
%%         It is started when reach_ouc application initiates.
%% @end
%% ===========================================================================

-module(oacd_ouc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
                                  ?CHILD(ouc_db_redis, worker),
                                  ?CHILD(ouc_web, worker),
                                  ?CHILD(ouc_agent_profiles, worker),
                                  ?CHILD(ouc_agents, worker),
                                  ?CHILD(ouc_queues, worker),
                                  ?CHILD(ouc_calls, worker),
                                  ?CHILD(ouc_rstat, worker),
                                  ?CHILD(ouc_agent_report_job, worker)
                                  %?CHILD(ouc_mongo_agent_report_job, worker)
]} }.
