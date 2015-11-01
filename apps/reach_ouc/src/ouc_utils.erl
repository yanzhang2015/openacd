%% ===========================================================================
%% @copyright 2014 eZuce Inc.
%% @author Costin-Tiberiu Radu <cradu@ezuce.com>
%% @doc Miscelaneous utility functions
%% @end
%% ===========================================================================

-module(ouc_utils).
%% Node identification utilitites
-export([get_nodes/0,
         get_current_node/0,
         get_node/1,
         get_atom_node/1,
         get_domain/1]).


%% --------------------------------------------------------------------------------
-spec get_nodes() -> list(binary()).
%% @doc Returns a list of binary strings with the names of all reach nodes.
%%      We assume one reach instance is active per node and
%%      as such we return the hostname of that node.
%% @end
%% --------------------------------------------------------------------------------
get_nodes() ->
    Nodes = lists:flatten(nodes(),[node()]),
    ReachNodes = lists:map(fun get_domain/1, Nodes),
    lager:debug("The list of reach nodes is: ~p ",[ReachNodes]),
    ReachNodes.



%% --------------------------------------------------------------------------------
-spec get_current_node() -> list(binary()).
%% @doc Returns a binary string with the name of the current reach node.
%% @end
%% --------------------------------------------------------------------------------
get_current_node() ->
    FullNode = node(),
    CurrentNode = get_domain(FullNode),
    lager:debug("The current node is: ~p ",[CurrentNode]),
    CurrentNode.

%% --------------------------------------------------------------------------------
-spec get_node(Pid::pid()) -> binary().
%% @doc Returns a binary string with the name of the reach node of the argument pid.
%% @end
%% --------------------------------------------------------------------------------
get_node(Pid) when is_pid(Pid) ->
    FullNode = node(Pid),
    Node = get_domain(FullNode),
    %lager:debug("The node where the process ~p resides is: ~p ",[Pid, Node]),
    Node.


%% ---------------------------------------------------------------------------------
-spec get_atom_node(Pid::pid()) -> atom().
%% @doc Returns an atom with the name of the reach node of the argument pid
%% @end
%% ---------------------------------------------------------------------------------
get_atom_node(Pid) when is_pid(Pid) -> 
    FullNode = node(Pid),
    Node = get_domain(FullNode),
    AtomNode = erlang:binary_to_atom(Node, utf8),
    AtomNode.

%% --------------------------------------------------------------------------------
-spec get_domain(NodeName::atom()) -> Domain::binary().
%% @doc
%% @end
%% --------------------------------------------------------------------------------
get_domain(NodeName) when is_atom(NodeName) ->
    BinName = erlang:atom_to_binary(NodeName, utf8),
    SplitName = binary:split(BinName, <<"@">>),
    [_Name, Domain] = SplitName,
    Domain.
