-module(user_default).

-export([
	rld/0
]).

rld() ->
	reloader:reload_modules(reloader:all_changed()).
