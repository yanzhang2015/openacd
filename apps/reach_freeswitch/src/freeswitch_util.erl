%% ===========================================================================
%%% @author Costin-Tiberiu Radu <cradu@ezuce.com>
%%% @author Nicoleta Lazar <nlazar@ezuce.com>
%%% @copyright (C) 2014, eZuce Inc.
%%% @doc Freeswitch utils
%%% @end
%% ===========================================================================
-module(freeswitch_util).

%% API
-export([get_ring_timeout/1,
         escape_string/1,
         escape_char/1]).


%% Default Ring timeout macros in seconds
-define(DEFAULT_RING_TIMEOUT, 12).

%% MongoDB agent parameters
-define(RING_TIMEOUT_PARAM,<<"ringtmout">>).
-define(AG_GROUP_PARAM,<<"aggrp">>).


%% -------------------------------------------------------------------------------------
-spec get_ring_timeout(Agent::list()) -> RingTimeout::integer().
%% @doc Returns the ring timeout for an agent. <br />
%%      Precendece rule: agent timeout > agent_group timeout > default<br />
%%      The default ring timeout should be 12 seconds.
%% @end
%% -------------------------------------------------------------------------------------
get_ring_timeout(Agent) ->
  RingTimeout = case get_ring_timeout(agent, Agent) of
                  {undefined, AgentGroup} ->
                    GrTimeout = case get_ring_timeout(group, AgentGroup) of
                                  undefined -> ?DEFAULT_RING_TIMEOUT;
                                  GrValue -> GrValue
                                end,
                    GrTimeout;
                  {AgTimeout, _AgentGroup} -> AgTimeout
                end,
  case is_float(RingTimeout) of
    true -> trunc(RingTimeout);
    false -> RingTimeout
  end.

%% -------------------------------------------------------------------------------------
%% @private
%% @doc Queries the MongoDB for the ring timeout.<br />
%%      First parameter specifies if this is done for agent or agentgroup.
%% @end
%% -------------------------------------------------------------------------------------
get_ring_timeout(agent, Name) ->
  JsonObj     = get_mongo_record(agent, Name),
  RingTimeout = case get_json_param(JsonObj, ?RING_TIMEOUT_PARAM) of
                    0 -> undefined;
                    undefined -> undefined;
                    RingT -> RingT
                end,
  AgGroup     = get_json_param(JsonObj, ?AG_GROUP_PARAM),
  {RingTimeout, AgGroup};

get_ring_timeout(group, Name) ->
  %% Name is_binary ... or convert?
  JsonObj     = get_mongo_record(group, Name),
  RingTimeout = get_json_param(JsonObj, ?RING_TIMEOUT_PARAM),
  RingTimeout.


%% --------------------------------------------------------------------------------------
%% @private
%% @doc Queries the MongoDB for agent or agentgroup records
%% @end
%% --------------------------------------------------------------------------------------
get_mongo_record(Type, Name) ->
  Db = ouc_db:get_db(imdb),
  EntityType = case Type of
                 agent -> <<"openacdagent">>;
                 group -> <<"openacdagentgroup">>;
                 _ -> <<"openacdagent">>
               end,
  {ok, JsonObj} = Db:findOne(<<"entity">>, [{<<"type">>, EntityType},{<<"name">>, Name}]),
  JsonObj.


%% ---------------------------------------------------------------------------------------
%% @private
%% @doc Gets a paramenter from a Json Object as returned from MongoDB
%% @end
%% --------------------------------------------------------------------------------------
get_json_param(JsonObj, Param) when is_list(JsonObj) ->
  Value = ej:get({Param}, JsonObj),
  Value;

get_json_param([], _Param) ->
  undefined.


%% ------------------------------------------------------------------------------------
%% @private
%% @doc Escape unwanted chars from CallerIDs
%% @end
%% ------------------------------------------------------------------------------------
-spec(escape_string(Input::list()) -> Output::list()).
escape_string(Input) ->
    PreList = lists:map(fun escape_char/1, Input),
    lists:flatten(PreList).


-spec(escape_char(Char::integer()) -> list()).
escape_char(Char) when Char =:= $&; Char =:= $.; Char =:= $-; Char =:= $_; Char >= 256 ->
    [$\\, Char];

escape_char(Char) when Char =< $/ ->
    $-;

escape_char(Char) when Char >= $:, Char =< $@ ->
    $-;

escape_char(Char) when Char >=$[, Char =< $` ->
    $-;

escape_char(Char) when Char >=${, Char =< 127 ->
    $-;

escape_char(Char) -> Char.
