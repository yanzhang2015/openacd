%% ===========================================================================
%%% @author Nicoleta Lazar <nlazar@ezuce.com>
%%% @copyright (C) 2014, eZuce Inc.
%%% @doc Gen media utils
%%% @end
%% ===========================================================================
-module(gen_media_util).

-include("call.hrl").

%% API
-export([expand_magic_skills/3]).

%% --------------------------------------------------------------------
-spec(expand_magic_skills/3 :: (QueueName :: list(),
                                CallRecord :: #call{},
                                Skills :: [atom()]) -> [atom()]).
%% @doc Expands the list of skills given as parameter with data
%%  provided by the call record and queue name.
%%  For example:
%%    '_node'    => {'_node', NodeName}
%%    '_queue'   => {'_queue', QueueName}
%%    '_brand'   => {'_brand', ClientName}
%%    OtherSkill => OtherSkill
%% @end
%% --------------------------------------------------------------------
expand_magic_skills(QueueName, CallRecord, Skills)
  when is_record(CallRecord, call)->
  Unfiltered = [case Skill of
                  '_node' -> {Skill, node(CallRecord#call.source)};
                  '_queue' -> {Skill, QueueName};
                  '_brand' ->
                    case CallRecord#call.client of
                      Client when is_record(Client, client) -> {Skill, Client#client.label};
                      _ -> {Skill, undefined}
                    end;
                  _ -> Skill
                end	|| Skill <- Skills],
  [X || X <- Unfiltered, X =/= []].
