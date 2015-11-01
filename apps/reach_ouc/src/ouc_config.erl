%% ===========================================================================
%% @copyright 2014 eZuce Inc.
%% @author Costin-Tiberiu Radu <cradu@ezuce.com>
%% @doc Config files processing functions - and the config files are proplists
%% @end
%% ===========================================================================
-module(ouc_config).
-export([
    get_property/3,
    get_options/2
]).

%% ---------------------------------------------------------------------------
-spec get_property(atom(), list(tuple()), atom()) -> atom().
%% @doc Gets a property from a proplist, with a default value
%%      if the requested property is undefined.
%% @end
%% ---------------------------------------------------------------------------
get_property(Property, PropList, Default) ->
    Backend = try proplists:get_value(Property, PropList, Default) of
        Val -> Val
    catch Error:Exception ->
        lager:error("Error on getting the property <~p> from list ~p due to ~p and ~p",[Property, PropList, Error, Exception]),
        Default
    end,
    Backend.

%% ---------------------------------------------------------------------------
-spec get_options(atom(), atom()) -> [tuple()].
%% @private
%% @doc Get config options from sys.config for an application and a module
%% @end
%% ---------------------------
get_options(Application, Module) ->
    Answer = try application:get_env(Application, Module) of
		{ok, V} -> V;
		_ -> []
    catch Error:Exception ->
        lager:error("Error on getting the option <~p> for the application ~p due to ~p and ~p",[Module, Application, Error, Exception]),
        []
    end,
    Answer.


