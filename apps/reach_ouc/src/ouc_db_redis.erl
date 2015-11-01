%% ===========================================================================
%% @copyright 2014 eZuce Inc.
%% @author Costin-Tiberiu Radu <cradu@ezuce.com>
%% @doc Redis db client - worker
%% @end
%% ===========================================================================

-module(ouc_db_redis).
-behaviour(gen_fsm).

%% API
-export([start/0,
         start_link/0, 
         get_connection/0,
         verify_connection/0,
         req/2, 
         req/3,
         get_state/0,
         close_connection/0]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%% FSM states
-export([disconnected/2,
         disconnected/3,
         connected/2,
         connected/3]).

%% Number of miliseconds between retrying to connect to the redis server in 
%% the case of a failure.
-define(CONNECT_TIMEOUT, 250).
%% 500 ms before 0 is returned as a result of a redis request
%% -define(SEND_MSG_TIMEOUT, 500).

%% Default redis hostname:port pair.
%% These are the bind port and bind ip for redis, on default install.
-define(DEFAULT_HOST,"127.0.0.1").
-define(DEFAULT_PORT,6379).

%% The connection member designates the eredis connection object
-record(state, {host = ?DEFAULT_HOST,
                port = ?DEFAULT_PORT,
                requests = [],
                connection
                }).

%% @todo Change the default port in order to not conflict with the SQA redis instance

%%%===================================================================
%%% API
%%%===================================================================

%% --------------------------------------------------------------------
-spec start_link() -> {ok, Pid::pid()} | ignore | {error, Error::term()}.
%% @doc
%% Creates a gen_fsm process which calls ouc_db_redis:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until ouc_db_redis:init/1 has returned.
%% Called by the supervisor.
%%
%% @end
%% --------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%% --------------------------------------------------------------------
-spec start() -> {ok, Pid::pid()} | ignore | {error, Error::term()}.
%% @doc
%% Creates a gen_fsm process which calls ouc_db_redis:init/1 to
%% initialize. <br />
%% Creates a standalone ouc_db_redis process, not part of a supervisor tree.
%% Used for testing.
%% @end
%% --------------------------------------------------------------------

start() ->
    gen_fsm:start({local, ?MODULE}, ?MODULE, [], []).



%% ---------------------------------------------------------------------------
-spec get_connection() -> {?MODULE, SelfPid ::pid() }.
%% @doc Returns the name of the current module and the pid of the current
%%      process.
%% @end
%% ---------------------------------------------------------------------------
get_connection() ->
    %% @todo write the pid of the started process in a ets table
    {?MODULE, self()}.


%% ---------------------------------------------------------------------------
-spec req(Type:: read | write, Query::list()) -> Result::term().
%% @doc Makes a redis request of a certain type. <br />
%%      Depending on type, a different type of result is returned.
%%      Allowed types of requests: read, write
%% @end
%% ---------------------------------------------------------------------------

req(write, RedisRequest) when is_list(RedisRequest) ->
    Request = {request, {write, RedisRequest}},
    case gen_fsm:sync_send_event(?MODULE, Request) of
        {ok, ok} -> {ok, 1};
        {ok, Card} -> {ok, Card};
        fail -> failure;
        partial -> 'partial-failure'
    end;

req(read, RedisRequest) when is_list(RedisRequest) ->
    Request = {request, {read, RedisRequest}},
    Result = gen_fsm:sync_send_event(?MODULE, Request),
    Result.

%% ---------------------------------------------------------------------------
%% @doc Makes a read request to redis and<br />
%%      parses the result depending on the supplied data type.<br />
%%      Data types: int, float, string (default)
%% @end
%% ---------------------------------------------------------------------------
req(read, RedisRequest, DataType) when is_list(RedisRequest) ->
    InitialResult = req(read, RedisRequest),
    process_result(InitialResult, DataType).

process_result(InitialResult, DataType) when is_binary(InitialResult) ->
    Result = case DataType of
        int   -> bin_to_int(InitialResult);
        float -> bin_to_float(InitialResult);
        string -> binary:bin_to_list(InitialResult)
    end,
    Result;

process_result(InitialResult, DataType) when is_list(InitialResult) ->
    Result = case InitialResult of
        [] -> [];
        List ->
            case DataType of
                int -> lists:map(fun bin_to_int/1, List);
                float -> lists:map(fun bin_to_float/1, List);
                string -> lists:map(fun binary:bin_to_list/1, List)
            end
    end,
    Result;

process_result(InitialResult, DataType) when is_atom(InitialResult) ->
    Result = case InitialResult of
        'undefined' ->
            case DataType of
                int -> -1; % defaulting to -1 may not be the best solution
                float -> -1.0;
                string -> ""
            end
    end,
    Result.

bin_to_int(Input) when is_binary(Input) ->
    {Result,[]} = string:to_integer(binary:bin_to_list(Input)),
    Result.

bin_to_float(Input) when is_binary(Input) ->
   {Result, []} = string:to_float(binary:bin_to_list(Input)),
    Result.

%% ----------------------------------------------------------------------------
-spec get_state() -> State::atom().
%% @doc Get the current state
%% @end
%% ----------------------------------------------------------------------------
get_state() ->
    try 
        gen_fsm:sync_send_event(?MODULE, get_state)
    catch
        exit : {noproc, _} -> closed
    end.


%% ---------------------------------------------------------------------------
-spec verify_connection() -> {ok, down}.
%% @doc Checks if the redis connection is really established
%% @end
%% ---------------------------------------------------------------------------
verify_connection() ->
    Result = ?MODULE:req(write, ["echo", "12345"]),
    case Result of
        {ok, 12345} -> ok;
        Other ->
            lager:warning("The redis connection does not verify due to ~p ", [Other]),
            down
    end.

%% ---------------------------------------------------------------------------
-spec close_connection() -> ok.
%% @doc closes the connection
%% @end
%% ---------------------------------------------------------------------------
close_connection() ->
    gen_fsm:send_all_state_event(?MODULE, stop).
%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
-spec init(Args::list()) -> {ok, disconnected, State::term()} |
                            {ok, disconnected, State::term(), Timeout::integer()}.
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
init(_Args) ->
    RedisCfg = ouc_config:get_options(reach_ouc, redis),
    lager:debug("...redis cfg from sys.config: ~p",[RedisCfg]),

    Host     = ouc_config:get_property(host, RedisCfg, ?DEFAULT_HOST),
    Port     = ouc_config:get_property(port, RedisCfg, ?DEFAULT_PORT),
    lager:info("...initiating redis connection on: (~p,~p)",[Host,Port]),

    gen_fsm:send_event(?MODULE, connect),
    {ok, disconnected, #state{host = Host,
                              port = Port}}.



%% ===================================================================
%% Async state callbacks
%% ===================================================================
%%--------------------------------------------------------------------
-spec disconnected(Event::atom() | term(), State::term()) -> 
            {next_state, connected | disconnected, State::term()}.
%% @private
%% @doc The connection to the redis server was not yet initialized or it has failed.
%% @todo Treat the case when the fsm is in the state disconnected and a request arrives 
%% @end
%%--------------------------------------------------------------------
disconnected(connect, State) ->
    lager:debug(" ... in disconnected state .... connecting ...."),
    RedisHost = State#state.host,
    RedisPort = State#state.port,
    case connect_to_redis(RedisHost, RedisPort) of
        {ok, Connection} -> 
            ConnectedState = State#state{connection = Connection},
            lager:debug(" ... in disconnected - just connected to redis ..."),
            {next_state, connected, ConnectedState};
        Error ->
            lager:warning(" ... in disconnected - failed to connect to redis due to ~p .. retrying",[Error]),
            timer:sleep(?CONNECT_TIMEOUT),
            {next_state, disconnected, State}
    end;

disconnected({'EXIT',Pid, Msg}, State) ->
    lager:error("The depending pid ~p sent us an EXIT with msg ~p",[Pid, Msg]),
    {next_state, disconnected, State};

disconnected(_Any, State) ->
    {next_state, disconnected, State}.




%% -------------------------------------------------------------------------
-spec connected(Event::atom() | term(), State::term()) ->
    {next_state, connected | disconnected, State::term()}.
%% @private
%% @doc Connected state; dealing with async messages
%% @end
%% -------------------------------------------------------------------------
connected(connect, State) ->
    lager:debug(" ... received `connect` request..."),
    case verify_connection() of
        ok ->
            lager:debug(" ... connection ok ..."), 
            {next_state, connected, State};
        down -> 
            lager:notice(" ...connection down; reconnecting ..."),
            RedisHost = State#state.host,
            RedisPort = State#state.port,
            case connect_to_redis(RedisHost, RedisPort) of
                {ok, Connection} -> 
                    ConnectedState = State#state{connection = Connection},
                    lager:debug(" ... just connected to redis ..."),
                    {next_state, connected, ConnectedState};
                Error ->
                    lager:warning(" ... failed to connect to redis due to ~p .. retrying",[Error]),
                    timer:sleep(?CONNECT_TIMEOUT),
                    {next_state, disconnected, State}
            end
    end;

connected(Event, State) ->
    lager:warning(" ... received unexpected event ~p with the state being ~p ",[Event, State]),
    {next_state, connected, State}.


%% =========================================================================
%% Sync state callbacks
%% =========================================================================


%% -------------------------------------------------------------------------
-spec disconnected(Event::atom() | term(), From::pid(), State::term()) ->
        {reply, Reply::term(), connected | disconnected, State::term()}|
        {next_state, connected, disconnected, State::term()}.
%% @private
%% @doc The connection to the redis server was not yet initialized or it has failed.
%% @todo Treat the case when the fsm is in the state disconnected and a request arrives
%% @end
%%--------------------------------------------------------------------

disconnected(connect, _From, State) ->
    lager:debug(" ... in disconnected state .... connecting ...."),
    RedisHost = State#state.host,
    RedisPort = State#state.port,
    case connect_to_redis(RedisHost, RedisPort) of
        {ok, Connection} -> 
            ConnectedState = State#state{connection = Connection},
            lager:debug(" ... in disconnected - just connected to redis ..."),
            {reply, connected, connected, ConnectedState};
        Error ->
            lager:warning(" ... in disconnected - failed to connect to redis due to ~p .. retrying",[Error]),
            timer:sleep(?CONNECT_TIMEOUT),
            {next_state, disconnected, State}
    end;


disconnected(get_state, _From, State) ->
    {reply, disconnected, disconnected, State};

disconnected({request, {write, Request}}, _From, State) ->
    %% on each new read / write request try to reconnect
    timer:sleep(?CONNECT_TIMEOUT),
    gen_fsm:send_event(?MODULE, connect), 
    %% Solution for queuing requests  
    %% QueuedRequests = State#state.requests ++ {write, Request},
    %% NewState = State#state{requests = QueuedRequests}
        
    lager:debug(" ... in disconnected state ... received the write request ~p ", [Request]),
    Reply = {ok, <<"0">>},
    {reply, Reply, disconnected, State};


disconnected({request, {read, Request}}, _From, State) ->
    [ReqMethod|_] = Request,
    Reply = case ReqMethod of
        <<"zrangebyscore">> -> [];
        _Other -> <<"0">>
    end,
    lager:debug(" ... in disconnected state ... received the read request ~p ", [Request]),
    timer:sleep(?CONNECT_TIMEOUT),
    gen_fsm:send_event(?MODULE, connect), 
    {reply, Reply, disconnected, State};


disconnected({'EXIT',Pid, Msg}, _From, State) ->
    lager:error("The depending pid ~p sent us an EXIT with msg ~p",[Pid, Msg]),
    {next_state, disconnected, State};


disconnected(timeout, _From, State) ->
    gen_fsm:send_event(?MODULE, connect), 
    timer:sleep(?CONNECT_TIMEOUT),
    {next_state, disconnected, State};

disconnected(_Any, From, State) ->
    lager:debug("In disconnected _Any~n"),
    disconnected(connect, From, State).



%%--------------------------------------------------------------------
-spec connected(Request::term(), From::pid(), State::term()) ->
    {reply, Result::term(), connected | disconnected, State}|
    {next_state, connected | disconnected, State}.
%% @private
%% @doc Connected state - only in this state redis request can be processed
%% @end
%%--------------------------------------------------------------------
connected({request, {read, RedisRequest}}, From, State) ->
    Conn = State#state.connection,
    case eredis:q(Conn, RedisRequest) of
        {ok, Result} ->
            {reply, Result, connected, State};
        {error, Reason } ->
            lager:error("Error making the redis request ~p because ~p",[RedisRequest, Reason]),
            %io:format("Error making the redis request ~p because ~p",[RedisRequest, Reason]),

            % We reconnect, but we lose the request
            [ReqMethod|_] = RedisRequest,
            Reply = case ReqMethod of
                <<"zrangebyscore">> -> [];
                _Other -> <<"0">>
            end,

            gen_fsm:reply(From, Reply),
            {next_state, disconnected, State}
    end;

connected({request, {write, RedisRequest}}, From, State) ->
    Conn = State#state.connection,
    case eredis:q(Conn, RedisRequest) of
        {ok, <<"OK">>} ->
            {reply, {ok, ok}, connected, State};

        {ok, BinaryAnswer} when is_binary(BinaryAnswer) ->
            StringAnswer = binary_to_list(BinaryAnswer),
            try list_to_integer(StringAnswer) of
                Cardinal -> {reply, {ok, Cardinal}, connected, State}
            catch
                _Exception:_Reason -> {reply, {ok, StringAnswer}, connected, State}
            end;

        {ok, OtherRes} ->
            lager:notice("The redis request ~p returned ok and ~p ", [RedisRequest, OtherRes]),
            {reply, ok, connected, State};

        {error, Reason } ->
            lager:error("Error making the redis request ~p because ~p",[RedisRequest, Reason]),

            % We reconnect, and answer a fake answer to the requester 
            gen_fsm:reply(From, {ok, 0}),
            {next_state, disconnected, State}
    end;


connected(get_state, _From, State) ->
    {reply, connected, connected, State};

connected(Event, _From, State) ->
    lager:notice("Unexpected event - ~p -  received while in state 'connected'", [Event]), 
    %io:format("Unexpected event - ~p -  received while in state 'connected'", [Event]), 
    {next_state, connected, State}.

%%--------------------------------------------------------------------
-spec handle_event(Event::term(), StateName::atom(), State::term()) ->
                   {next_state, StateName::atom(), State::term()}.
%% @private
%% @doc Handles unexpected async events - log a notice with lager and keep the state.
%% @end
%%--------------------------------------------------------------------
handle_event(stop, _StateName, State) ->
    close_connection(State#state.connection),
    {stop, normal, State};

handle_event(Event, StateName, State) ->
    lager:notice("The ~p, in state ~p, received an unexpected async event: ~p ", [?MODULE, StateName, Event]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
-spec handle_sync_event(Event::term(), From::pid(), StateName::atom(), State::term()) -> 
            {reply, ok, StateName::atom(), State::term()}.
%% @private
%% @doc Handles unexpected sync events - log a notice with lager and keep the state.
%% @end
%%--------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, State) ->
    lager:notice("The ~p, in state ~p, received an unexpected sync event: ~p ", [?MODULE, StateName, Event]),
    %io:format("The ~p, in state ~p, received an unexpected sync event: ~p ", [?MODULE, StateName, Event]),

    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
-spec handle_info(Info::term(), StateName::atom(), State::term())->
                   {next_state, StateName::atom(), State::term()} .
%% @private
%% @doc Handles any message other than a synchronous or asynchronous event
%% (or a system message).
%% @end
%%--------------------------------------------------------------------
handle_info(Info, StateName, State) ->
    lager:notice("The ~p, in state ~p, received an unexpected message: ~p ", [?MODULE, StateName, Info]),
    %io:format("The ~p, in state ~p, received an unexpected message: ~p ", [?MODULE, StateName, Info]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
-spec terminate(_Reason, _StateName, _State) -> ok.
%% @doc Shutting down the redis client. 
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, State) ->
    close_connection(State#state.connection),
    ok.

%% --------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
-spec code_change(_OldVsn, StateName, State, _Extra) ->
                   {ok, StateName, State}.
%% @end
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ===================================================================
%%  Internal functions
%% ===================================================================

connect_to_redis(RedisHost, RedisPort) 
    when is_list(RedisHost),
         is_integer(RedisPort) ->

    lager:debug("in connect_to_redis (~p,~p)",[RedisHost, RedisPort]),

    process_flag(trap_exit, true),
    try eredis:start_link(RedisHost, RedisPort) of
        {ok, RedisConn} -> 
            lager:debug("eredis process ~p connected to ~p on ~p", [RedisConn, RedisHost, RedisPort]),
            {ok, RedisConn};
        {error, Reason} ->
            lager:error("Error connecting to Redis on host: ~s and port: ~B due to ~p", 
                            [RedisHost, RedisPort, Reason]),
            {error, nocon}
    catch
        Exception -> 
            lager:error("Error thrown on connnecting to Redis on (~p:~p) due to ~p",
                          [RedisHost, RedisPort, Exception]),
            {error, nocon}
    
    end. 


%% -------------------------------------------------------------------
-spec close_connection(Connection::pid()) -> ok.
%% @private
%% @doc Close the redis connection in a nice way.
%% @end
%% -------------------------------------------------------------------
close_connection(Connection) ->
    eredis:stop(Connection).
