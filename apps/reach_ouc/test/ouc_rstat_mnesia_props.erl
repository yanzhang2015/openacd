-module(ouc_rstat_mnesia_props).

% -export([command/1, precondition/2, next_state/3, postcondition/3]).
-compile([export_all]).

-include_lib("proper/include/proper.hrl").
-include("ouc_rstat.hrl").

-define(SERVER, ouc_rstat_mnesia).

-record(state, {confs, now, calls=[]}).
% -record(call, {client, start_i, end_i, ans_duration}).

-define(CLIENTS, ["bdo", "bpi", "cityland", "jollibee", "kfc", "subspace"]).
-define(QUEUE, ["support", ""]).

prop_call_duration_works_fine() ->
	?FORALL(Confs, confs(),
	?FORALL(Cmds, commands(?MODULE, initial_state(Confs)),
		?TRAPEXIT(
		begin
			crypto:start(),
			mnesia:start(),
			ouc_rstat_mnesia:install(),
			mnesia:clear_table(ouc_rstat),

			{History,State,Result} = run_commands(?MODULE, Cmds),
			?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
				[History,State,Result]),
			Result =:= ok)
		end))).

confs() ->
	list(
		union([
			?LET(UpdateLen, pos_integer(),
			?LET(Times, pos_integer(),
				{dynamic, atom(),
					{minute, UpdateLen * Times},
					{minute, UpdateLen}, ?DEFAULT_PIVOT})
			),

			{static, atom(), pos_integer(), ?DEFAULT_PIVOT}
		])
	).

client() ->
	elements(?CLIENTS).

ans_duration() ->
	range(0, 1000).

agent() ->
	"agent".

log_call(S) ->
	Confs = S#state.confs,
	Now = S#state.now,

	?LET({ActiveDur, AnsDur, Delay}, {pos_integer(), pos_integer(), default(0, integer(-1, 10))},
		begin
			CallDur = ActiveDur + AnsDur,
			End = Now - Delay, %% TODO simulate delay, ffwd
			Start = End - CallDur,

			{call, ?SERVER, log_call, [agent(), client(), Start, End, AnsDur, Confs]}
		end).

subtract(X, Y) -> X - Y.

instant() ->
	?SIZED(Size, resize(Size * 2, non_neg_integer())).

command(S) ->
	Confs = S#state.confs,
	% io:format("Command state: ~p~n", [S]),

	weighted_union(
		[{max(1, length(Confs) * 9), log_call(S)}] ++ %% chosen 9 out of 10 times
		[{1, {call, ?SERVER, get_prop, [?PROP_TE_CALL_DURATION, instant(), Conf, Client]}} || Conf <- Confs, Client <- ?CLIENTS]
	).

initial_state(Confs) ->
	#state{
		confs = Confs,
		now = 1000
	}.

precondition(_, _) ->
	true.

next_state(S, _V, {call, _, log_call, [_, Client, Start, End, Ans, _]}) ->
	Call = {call, Client, Start, End, Ans},
	Calls = S#state.calls,
	ff_state(S#state{calls = [Call|Calls]});
next_state(S, _V, _) ->
	ff_state(S).

ff_state(#state{now=N} = S) ->
	F = crypto:rand_uniform(0,10),
	S#state{now=(N + F)}.

postcondition(S, {call, _, get_prop, [?PROP_TE_CALL_DURATION, I, Conf, Client]}, R) ->
	% io:format("Post state: ~p~n", [S]),
	Intvs = ouc_rstat_util:get_covered_intervals(I, Conf),
	[{F, _}|_] = Intvs,
	{_, T} = lists:last(Intvs),

	CallDurations = [End - Start || {call, Cl, Start, End, _Ad} <- S#state.calls,
		Cl =:= Client, End >= F, End =< T],
	Total = lists:sum(CallDurations),
	% io:format("~p vs ~p~n", [Total, R]),
	Total =:= R;
postcondition(_S, C, _) ->
	io:format("Call: ~p~n", [C]),
	true.