-module(reach_config_reloader).
-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

-define(CHECK_INTERVAL, 10000).

-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(config, {
	file_logger
}).

-record(state, {
	last :: tuple(),
	config_path :: string(),
	config :: #config{},
	tref :: reference()
}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

%% gen_server callbacks

init([]) ->
	case os:getenv("LOGCFG_PATH") of
		false ->
            lager:warning("LOGCFG_PATH environment variable undefined, stopping"),
			{stop, normal};
		Filename ->
			lager:info("Reading config from ~p", [Filename]),
			file:read_file_info(Filename),
			{ok, [Env]} = file:consult(Filename),
			Config = get_config(Env),
			load_config(undefined, Config),

			{ok, Tref} = timer:send_interval(?CHECK_INTERVAL, check_config),
			{ok, #state{last = stamp(), config_path = Filename, config = Config, tref = Tref}}
	end.

handle_call(stop, _From, State) ->
	{stop, normal, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(check_config, State) ->
	#state{last = From, config_path = Filename, config = OldConfig} = State,
	To = stamp(),
	case file:read_file_info(Filename) of
		{ok, #file_info{mtime = Mtime}} when Mtime >= From, Mtime < To ->
			{ok, [Env]} = file:consult(Filename),
			NewConfig = get_config(Env),
			load_config(OldConfig, NewConfig),
			{noreply, State#state{last = To, config = NewConfig}};
		_ ->
			{noreply, State}
	end;
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, #state{tref = Tref}) ->
	timer:cancel(Tref),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal functions

-spec get_config(Env :: list({atom(), list()})) -> #config{}.
get_config(Env) ->
	#config{
		file_logger = get_config(file_logger, Env)
	}.

get_config(file_logger, Env) ->
	LagerConf = proplists:get_value(lager, Env, []),
	Handlers = proplists:get_value(handlers, LagerConf, []),
	LogFileConf = proplists:get_value(lager_file_backend, Handlers, []),
	Path = proplists:get_value(file, LogFileConf),
	Level = proplists:get_value(level, LogFileConf),
	{Path, Level}.

-spec load_config(undefined | #config{}, #config{}) -> ok.
load_config(Config, Config) ->
	ok;
load_config(_, NewConfig) ->
	{Path, Level} = NewConfig#config.file_logger,
	lager:set_loglevel(lager_file_backend, Path, Level),
	ok.

stamp() ->
	erlang:localtime().
