-module(oacd_ouc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	application:start(lager),
    application:start(cowboy),
	ok = ouc_db:init(),
    timer:apply_after(5000, ouc_call_recording_migration, start, []),
    oacd_ouc_sup:start_link().

stop(_State) ->
    ok.
