%% ===========================================================================
%% @copyright 2014 eZuce Inc.
%% @author Costin-Tiberiu Radu <cradu@ezuce.com>
%% @doc Rolling stats backend in redis
%% @end
%% ===========================================================================
-module(ouc_rstat_store_redis).
-behaviour(ouc_rstat_store).

-include("ouc_rstat.hrl").

%% -------------------------------------------
%% @todo what is the purpose of the TAB and VSN macros ?
%% -------------------------------------------
-define(TAB, ouc_rstat).
-define(VSN, 3).

%% ===========================================================================
%% Public API
%% ===========================================================================

-export([
    get_store/0,
    init/1,
    increment/3,
    set_if_max/3,
    read/2,
    read_range/2,
    needs_cache/0
]).


%% ---------------------------------------------------------------------------
-spec get_store() -> {ModuleName::atom(), void}.
%% @doc Returns the current store ?? 
%% @end
%% ---------------------------------------------------------------------------
get_store() ->
    {?MODULE, void}.


%% ---------------------------------------------------------------------------
-spec init(Arg::any()) -> {ok, void}.
%% @doc Initialise the connection to the local Redis server and 
%%      ensure the connection is active. 
%%      Returns {ok, void}
%% @todo replace ouc_db_master:get_connection(redis) with something like ouc_db:get_connection(redis) when the mongo client is separated
%% @end
%% --------------------------------------------------------------------------- 
init(_Arg) ->
    Db = ouc_db_master:get_connection(redis),

    %case Db:verify_connection() of
    %    ok      -> lager:info("Starting ouc_rstat_store_redis - connection to server is ok");
    %    down    -> lager:error("Error verifying the redis connection ")
    %end,
      
        
    try Db:verify_connection() of
         ok -> lager:info("Starting ouc_rstat_store_redis - connection to server is ok");
         down -> lager:error("Error verifying the redis connection ")
    catch
        Error:Reason -> lager:error("Error verifying the redis connection due to ~p ~p", [Error, Reason])
    end,
    {ok, void}.


%% ----------------------------------------------------------------------------
-spec increment(_Old, Key, Increment) -> void
    when 
        Key :: {Type, {From, To}, Nxds},
            Type :: tuple(),
            From :: instant(),
            To :: instant(),
            Nxds :: [{atom(), list()}],
        Increment :: integer().
%% @doc Increments the value of the key 'Key' with 'Increment'. <br />
%%      From and To are representing timestamps in unixts 
%% @end
%% @todo implement it
%% -----------------------------------------------------------------------------
increment(_OldArg, Key, Increment) ->
    {RStatInsert, RStatSearch, From, Cov} = key_to_redis_keys(Key),    
    Db = ouc_db_master:get_connection(redis), %% useless; should be stored in the state
    
    %% Incrementing key
    %% Key : rstat:i:<type>:<from>:<cov>:[<Indexes>]
    %% Type: string
    %% Val : Value (incremented)
    %% TTL: Cov * 2; 
    Expire = Cov * 2,
    % lager:debug("incrby ~p ~p", [RStatInsert, Increment]),
    % lager:debug("expire ~p ~p",[RStatInsert, Expire]),
    {ok, NewValue} = Db:req(write, [<<"incrby">>,RStatInsert, Increment]),
    {ok, _Res } = Db:req(write, [<<"expire">>, RStatInsert, Expire]),
            
    %% Searching key
    %% Key: rstat:s:<type>:<cov>:[<Indexes>]
    %% Score: <From> (timestamp)
    %% Member: <NewValue> (incremented above)
    
    %% zrem first the old value
    % lager:debug("zremrangebyscore ~p ~p ~p",[RStatSearch, From, From]),
    Db:req(write, [<<"zremrangebyscore">>, RStatSearch, From, From]),    

    % lager:debug("zadd ~p ~p ~p", [RStatSearch, From, NewValue]),
    Db:req(write, [<<"zadd">>,  RStatSearch, From, NewValue]),

    
    void.

%% ------------------------------------------------------------------------------
-spec set_if_max(_OldArg, Key, Increment) -> void
    when 
        Key :: {Type, {From, To}, Nxds},
        Type :: tuple(),
        From :: instant(),
        To :: instant(),
        Nxds :: [{atom(), list()}],
        Increment :: integer().
%% @doc The same as with increment, but now instead of incrementing with N, 
%%      N is the new value of the val member of the mongo document. 
%% @end
%% @todo implement
%% ---------------------------------------------------------------------------
set_if_max(_OldArg, Key, NewVal) ->
    {RStatInsert, RStatSearch, From, Cov} = key_to_redis_keys(Key),  
    Db = ouc_db_master:get_connection(redis), %% useless; should be stored in the state
    
    % lager:debug("get ~p ", [RStatInsert]),
    CurrentValue = case Db:req(read, [<<"get">>, RStatInsert]) of
        undefined -> 
            % lager:debug("the key ~p is undefined", [RStatInsert]),
            0;
        CurrentS when is_binary(CurrentS) ->    
            {CurrentSc, []} = string:to_integer(binary:bin_to_list(CurrentS)),
            CurrentSc;
        Other -> 
            lager:warning("get ~p returned unexpectedly ~p", [RStatInsert, Other]),
            0
        end,
    
    if NewVal > CurrentValue ->
        Expire = Cov * 2,
        % lager:debug("setex ~p ~p ~p", [RStatInsert, Expire, NewVal]),
        {ok, _Res} = Db:req(write, [<<"setex">>, RStatInsert, Expire, NewVal]),

        % first remove the old RStatSearch entry as it we might have duplicate entries
        % lager:debug("zremrangebyscore ~p ~p ~p", [RStatSearch, From, From]),
        Db:req(write, [<<"zremrangebyscore">>, RStatSearch, From, From]),

        % lager:debug("zadd ~p ~p ~p", [RStatSearch, From, NewVal]),
        Db:req(write, [<<"zadd">>, RStatSearch, From, NewVal]),
        true;
    true ->
        % lager:debug("The new score is not higher than the old one"),
        true
    end,  
    void.


%% ---------------------------------------------------------------------------
-spec read(_OldState::any(), Key) -> {void, Val::integer()}
    when 
        Key :: {Type, {From, To}, Nxds},
        Type :: tuple(),
        From :: instant(),
        To :: instant(),
        Nxds :: [{atom(), list()}].
%% @doc Returns a value in the db, given the Mongo query K 
%% @end
%% @todo implement
%% ---------------------------------------------------------------------------
read(_First, Key) ->
    {RStatInsert, _RStatSearch, _From, _Cov} = key_to_redis_keys(Key),    
    Db = ouc_db_master:get_connection(redis), %% useless; should be stored in the state
    
    % lager:debug("get ~p ", [RStatInsert]),
    ReturnVal = Db:req(read, [<<"get">>, RStatInsert]),
    
    Result = if 
        is_binary(ReturnVal) -> 
            {CurrentValue, []} = string:to_integer(binary_to_list(ReturnVal)),
            CurrentValue;
        is_atom(ReturnVal) -> 0; %undefined
        true -> 0
    end,   
    {void, Result}.


%% ---------------------------------------------------------------------------
-spec read_range(any(), Key) -> {void, Val::integer()}
    when 
        Key :: {Type, {From, To}, Nxds},
        Type :: tuple(),
        From :: instant(),
        To :: instant(),
        Nxds :: [{atom(), list()}].
%% @doc read_range currently does not take into account multiple
%%      rules with same coverage but different pivot
%% @todo implement
%% @end
%% ---------------------------------------------------------------------------
read_range(_OldArgs, Key) -> 
    {_RStatInsert, RStatSearch, From, To} = key_to_redis_patterns(Key),    
    Db = ouc_db_master:get_connection(redis), %% useless; should be stored in the state
    StatType = get_type(Key), %% max, total, other

    %% Incrementing key
    %% Key : rstat:i:<type>:<from>:<cov>:[<Indexes>]
    %% Type: string
    %% Val : Value (incremented)
    %% TTL: Cov * 2; 

    %% Searching key
    %% Key: rstat:s:<type>:<cov>:[<Indexes>]
    %% Score: <From> (timestamp)
    %% Member: <Value> (incremented above)

    % lager:debug("zrangebyscore ~p ~p ~p", [RStatSearch, From, To]),
    ListOfMatches = Db:req(read, [<<"zrangebyscore">>,RStatSearch, From, To]),
    
    Result = case ListOfMatches of 
        [] -> 0;
        Matches when is_list(Matches) ->
            ToIntFun = fun (BinVal) ->
                list_to_integer(binary:bin_to_list(BinVal)) 
            end,
            NumList = lists:map(ToIntFun, Matches),
            PartialResult = case StatType of
                max -> lists:max(NumList);
                total -> lists:sum(NumList);
                _ ->
                    lager:error("A read_range was requested for an unknown type"), 
                    error
            end,
            % lager:debug("stat-type: ~p => ~p", [StatType, PartialResult]),
            PartialResult;
        Other -> 
            lager:warning("read_range returned unexpectedly ~p", [Other]),
            0
    end,        
    
    {void, Result}.

%-define(DEFAULT_CONF, [
%   {dynamic, last_15m, {minute, 15}, 5},
%   {dynamic, last_30m, {minute, 30}, 5},
%   {dynamic, last_hr, {hour, 1}, 5},
%   {static, today, {day, 1}},
%   {static, this_week, {day, 7}},
%   {static, this_month, {month, 1}}
%]).


%% ---------------------------------------------------------------------------
%% @doc Just returns true
%% @end
%% ---------------------------------------------------------------------------
needs_cache() -> ok.

%% ===========================================================================
%% Internal functions
%% ===========================================================================

%% ---------------------------------------------------------------------------
-spec key_type_to_string(KeyType::tuple()) -> Result::string().
%% @doc Join the elements of a rolling stat key type to a into a string
%%      separated by ':'
%% @end
%% ---------------------------------------------------------------------------
key_type_to_string(KeyType) when is_tuple(KeyType)->
    StringList = [atom_to_list(Elem) || Elem <- tuple_to_list(KeyType)],
    TypeString = string:join(StringList, ":"),
    TypeString.



%% -----------------------------------------------------------------------------
-spec get_key_ndxs(Ndxs::list()) -> [Index::list()].
%% @doc parses the list of Ndxs and transforms it in a list of strings
%% @end
%% -----------------------------------------------------------------------------
get_key_ndxs(Ndxs) ->
    SortedIndexes = lists:sort(Ndxs),
    Result = [parse_ndx(IndexType, IndexValue) || {IndexType, IndexValue} <- SortedIndexes],
    Result.

parse_ndx(NdxsKey, NdxsVal) ->
    ToStringFun = fun(X) ->
        if
            is_list(X) -> X;
            is_atom(X) -> erlang:atom_to_list(X);
            is_integer(X) -> integer_to_list(X);
            true -> X
        end
    end,
    Result = try
        {ToStringFun(NdxsKey), ToStringFun(NdxsVal)} of
            Res -> Res
        catch
            Error:Reason -> lager:error("Failed to parse the Ndxs with key: ~p and val ~p due to ~p and ~p",[NdxsKey, NdxsVal, Error, Reason])
    end,
    Result.


%% ------------------------------------------------------------------------------
-spec key_to_redis_keys(Key::tuple()) -> 
        {RStatsInsert::string(), 
         RStatsSearch::string(), 
         From::integer(), 
         Cov::integer()}.
%% @doc Parses the Key variable as it is passed to the public API functions
%%      and returns two redis key names: <br />
%%      RStatsPrefix <br />
%%      RStatsSpecific <br />
%% @end
%% ------------------------------------------------------------------------------

key_to_redis_keys(Key) ->
    {Type, {From, To}, Ndxs} = Key,
    KeyType = key_type_to_string(Type),
    Indexes = get_key_ndxs(Ndxs),

    Cov = To - From + 1,
    RStatInsertPrefix = lists:flatten(["rstat:", KeyType, ":", io_lib:format("~p:~p", [From, Cov])]),
    RStatSearchPrefix = lists:flatten(["rstat:s:", KeyType, ":", io_lib:format("~p", [Cov])]),


    {RStatInsert, RStatSearch} = 
        if length(Indexes) > 0 ->
            %% io_lib:format("~s:~s",[IndexName, IndexValue])
            IndexesSuffix = lists:flatten([ io_lib:format(":~s:~s",[IndexName, IndexValue]) || {IndexName, IndexValue} <- Indexes]), 
            %% IndexesSuffix = lists:flatten([[":", IndexName, ":", IndexValue] || {IndexName, IndexValue} <- Indexes]), 
            RSI = RStatInsertPrefix ++ IndexesSuffix, 
            RSS = RStatSearchPrefix ++ IndexesSuffix, 
            {RSI, RSS};    
        true -> {RStatInsertPrefix, RStatSearchPrefix}
        end,

    try lists:map(fun erlang:list_to_binary/1, [RStatInsert, RStatSearch]) of  
        [RStatInsertBin, RStatSearchBin] ->  
            {RStatInsertBin, RStatSearchBin, From, Cov}
    catch 
        Error:Reason ->
            lager:error("Error converting the key names ~p and ~p to bins because ~p ~p",
                    [RStatInsert,RStatSearch, Error, Reason]),
            {<<"err">>, <<"err:s">>, From, Cov}
    end. 
   


key_to_redis_patterns(Key) ->
    {Type, {From, To}, Cov, Ndxs} = Key,
    KeyType = key_type_to_string(Type),
    Indexes = get_key_ndxs(Ndxs),
    
    RStatInsertPrefix = lists:flatten(["rstat:", KeyType, ":", io_lib:format("~p:~p", [From, Cov])]),

    RStatSearchPrefix = lists:flatten(["rstat:s:", KeyType, ":", io_lib:format("~p", [Cov])]),

    {RStatInsert, RStatSearch} = 
        if length(Indexes) > 0 ->
            IndexesSuffix = lists:flatten([[":", IndexName, ":", IndexValue] || {IndexName, IndexValue} <- Indexes]), 
            RSI = RStatInsertPrefix ++ IndexesSuffix, 
            RSS = RStatSearchPrefix ++ IndexesSuffix, 
            {RSI, RSS};    
        true -> {RStatInsertPrefix, RStatSearchPrefix}
        end,
    {RStatInsert, RStatSearch, From, To}.



%% ------------------------------------------------------------------------------
-spec  get_type(Key::tuple()) -> max | total | other.
%% @doc Returns the type of the statistic we are looking for
%% @end
%% ------------------------------------------------------------------------------
get_type({{max, _, _}, _, _, _}) -> max;
get_type({{total, _, _}, _, _, _}) -> total;
get_type(_) -> other.


