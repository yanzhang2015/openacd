-module(reach).

-export([start/0]).

start() ->
	ensure_started(reach).

ensure_started(App) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, _}} -> ok;
		{error, {not_loaded, Dep}} ->
			ensure_started(Dep),
			ensure_started(App);
		{error, {not_started, Dep}} ->
			ensure_started(Dep),
			ensure_started(App)
	end.
