-module(ouc_db_redis_eunit).
-include_lib("eunit/include/eunit.hrl").

start_it_test() ->
    {ok, Pid} = ouc_db_redis:start(),
    log_msg("start-it-pid: ~p ~n", [Pid]),
    ?assert(is_pid(Pid)).


get_connection_test() ->
    {Mod, pid} = ouc_db_redis:get_connection(),
    ?assertEqual(Mod, ouc_db_redis).


write_string_test() ->
    {Mod, pid} = ouc_db_redis:get_connection(),
    Res = Mod:req(write,["set", "simple:string", "simple:othervalue"]),
    ?assertEqual({ok, 1},  Res).
    

read_string_test() ->
    Result = ouc_db_redis:req(read, ["get", "simple:string"]),
    ?assertEqual(<<"simple:othervalue">>, Result).



write_zset_test() ->
    Res = ouc_db_redis:req(write, ["zadd","zset:one",10,"value:one", 12, "value:two"]),
    ?assertEqual({ok, 2}, Res).

read_zset_test() ->
    Res = ouc_db_redis:req(read, ["zrange", "zset:one", 0, -1]),
    ?assertEqual([<<"value:one">>, <<"value:two">>], Res).



flushdb_test() ->
    Res = ouc_db_redis:req(write, ["flushdb"]),
    ?assertEqual({ok,1}, Res).


verify_connection_test() ->
    Answer = ouc_db_redis:verify_connection(),
    ?assertEqual(ok, Answer).

%% ----------------------------------------------------------------------------
%% Private func
%% ----------------------------------------------------------------------------
log_msg(Format, Vars ) ->
    {ok, Fout} = file:open("ouc_db_redis_eunit.log", [append]),
    io:fwrite(Fout, Format, Vars),
    file:close(Fout).
    


%% close_test() ->
%%     ?assert(ok =:= ouc_db_redis:close_connection()).
