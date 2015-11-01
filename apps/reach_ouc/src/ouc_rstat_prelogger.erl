%% one off task for logging prior to snapshot update

-module(ouc_rstat_prelogger).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
	fire/1,

	register/0,
	notify_logged/1
]).

-define(KEY, {p, g, ?MODULE}).
-define(EVENT, ouc_rstat_prelog).

-ifndef(TEST).
-define(TIMEOUT, 1000).
-else.
-define(TIMEOUT, 50).
-endif.

%% API

%% @doc start logging
fire(Ts) ->
	init(Ts).

%% @doc add the calling pid as a pre tick logger
register() ->
	gproc:reg(?KEY, ok).

notify_logged({Job, Ref}) ->
	Job ! {notify_logged, Ref}.

%% internal

init(Ts) ->
	TRef = erlang:start_timer(?TIMEOUT, self(), timeout),

	Self = self(),
	Pids = gproc:lookup_pids(?KEY),

	Mons = lists:map(fun(P) ->
		Mon = erlang:monitor(process, P),
		P ! {?EVENT, Ts, {Self, Mon}},
		Mon
	end, Pids),
	MonSet = sets:from_list(Mons),
	loop(MonSet, TRef).

loop(MonSet, TRef) ->
	case sets:size(MonSet) of
		0 ->
			done;
		_ ->
			receive
				{notify_logged, M} ->
					erlang:demonitor(M),
					MonSet1 = sets:del_element(M, MonSet),
					loop(MonSet1, TRef);
				{'DOWN', M, _Type, Pid, Info} ->
					%% not so nice way since this will catch
					%% monitors that may not have been for this process
					lager:warning("Rstat logging process ~p died: ~p",
						[Pid, Info]),
					MonSet1 = sets:del_element(M, MonSet),
					loop(MonSet1, TRef);
				{timeout, TRef, _} ->
					Mons = sets:to_list(MonSet),
					lists:foreach(fun(M) -> erlang:demonitor(M) end,Mons),
					{error, timeout}
			end
	end.

%% tests

-ifdef(TEST).

-define(M, ?MODULE).

clean_log_test() ->
	application:start(gproc),

	P1 = mock_clean_logger(),
	P2 = mock_clean_logger(),

	%% commence prelog
	?M:fire(ts()),

	?assert(is_logged()), %% logged once
	?assert(is_logged()), %% logged twice

	stop_logger(P1),
	stop_logger(P2).

die_test() ->
	application:start(gproc),

	P1 = mock_clean_logger(),
	P2 = mock_dying_logger(),

	%% commence prelog
	?M:fire(ts()),

	?assert(is_logged()), %% logged once
	?assertNot(is_logged()), %% and only once

	stop_logger(P1),
	stop_logger(P2).

timeout_test() ->
	application:start(gproc),

	P = mock_idle_logger(),
	?assertEqual({error, timeout}, ?M:fire(ts())),

	stop_logger(P).

stop_logger(P) ->
	catch cpx_dummy_pid:stop(P).

mock_clean_logger() ->
	mock_logger(fun(Self) ->
		receive
			{ouc_rstat_prelog, _Ts, Ref} ->
				Self ! logged,
				timer:sleep(50),
				?M:notify_logged(Ref)
		end
	end).

mock_dying_logger() ->
	mock_logger(fun(_) ->
		receive
			{ouc_rstat_prelog, _Ts, _Ref} ->
				exit(normal)
		end
	end).

mock_idle_logger() ->
	mock_logger(fun(_) -> nothing end).

mock_logger(F) ->
	Self = self(),

	P = cpx_dummy_pid:start(),
	cpx_dummy_pid:do(P, fun() -> ?M:register() end),

	cpx_dummy_pid:do_async(P, fun() -> F(Self) end),

	P.

is_logged() ->
	receive
		logged -> true
	after 0 ->
		false
	end.

ts() ->
	ouc_time:datetime_to_unixts({{2013, 5, 20}, {9, 0, 0}}).

-endif.
