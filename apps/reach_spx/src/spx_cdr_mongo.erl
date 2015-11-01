-module(spx_cdr_mongo).
-behavior(gen_cdr_dumper).

-include_lib("reach_core/include/call.hrl").
-include_lib("reach_core/include/agent.hrl").

-export([
	init/1,
	terminate/2,
	code_change/3,
	dump/2,
	commit/1,
	rollback/1
]).

-record(state, {
	agent_cache = dict:new()
}).

init(_Opts) ->
	try_ensure_ndxs(),
	{ok, #state{}}.

terminate(_Reason, _State) ->
	ok.

code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

dump(AgState, State) when is_record(AgState, agent_state) ->

	#agent_state{
		id = AgentId,
		agent = Agent,
		state = Ztate,
		oldstate = OldZtate,
		start = Start,
		ended = End,
		profile = Profile
	} = AgState,

	Cache = State#state.agent_cache,
	{Clients, Cache1} = get_clients(Agent, Cache),

	Entry = [
		{<<"vsn">>, <<"0.1.0">>},
		{<<"agent">>, fx(Agent)},
		{<<"agent_id">>, fx(AgentId)},
		{<<"profile">>, fx(Profile)},
		{<<"clients">>, {array, Clients}},
		{<<"old_state">>, fx(OldZtate)},
		{<<"state">>, fx(Ztate)},
		{<<"start">>, as_erl_now(Start)},
		{<<"end">>, as_erl_now(End)}
	],

	try_insert(<<"asl">>, Entry),

	{ok, State#state{agent_cache=Cache1}};
dump(CDR, State) when is_record(CDR, cdr_rec) ->
	Call = CDR#cdr_rec.media,

	Summary = CDR#cdr_rec.summary,
	Raws = CDR#cdr_rec.transactions,

	Agent = find_agent(Summary),
	Queue = find_queue(Summary),

	%% WARN: might not be the active profile when the call was made
	Profile = get_profile(Agent),

	CallId = Call#call.id,
	Type = Call#call.type,
	CallerId = case Call#call.callerid of
		{X, Y} -> X ++ Y;
		_ -> undefined
	end,
	Dnis = Call#call.dnis,
	Mod = Call#call.source_module,
	Direction = Call#call.direction,

	Client = case Call#call.client of
		#client{label=L} -> L;
		_ -> undefined
	end,


	Start = get_start(Raws),
	End = get_end(Raws),
	CallEnd = get_call_end(Raws),

	TRs = [{Tr, St} || #cdr_raw{transaction=Tr, start=St}<-Raws, is_atom(Tr),
		Tr =/= cdrinit, Tr =/= cdrend],

	Entry = [
		{<<"callid">>, fx(CallId)},
		{<<"start">>, as_erl_now(Start)},
		{<<"end">>, as_erl_now(End)},
		{<<"call_end">>, as_erl_now(CallEnd)},

		%% @todo -- agent, profile, queue will be arrays once transfer is enabled
		{<<"agent">>, fx(Agent)},
		{<<"profile">>, fx(Profile)},
		{<<"queue">>, fx(Queue)},

		{<<"client">>, fx(Client)},

		{<<"type">>, fx(Type)},
		{<<"caller_id">>, fx(CallerId)},
		{<<"dnis">>, fx(Dnis)},
		{<<"mod">>, fx(Mod)},
		{<<"direction">>, fx(Direction)},

		{<<"transactions">>,
			{array, [[{<<"state">>, fx(Tr)},
				{<<"ts">>, as_erl_now(St)}] || {Tr, St} <- TRs]}},

		{<<"vsn">>, <<"0.1.1">>}
	],
	try_insert(<<"cdr">>, Entry),
	{ok, State}.

commit(State) ->
	{ok, State#state{agent_cache=dict:new()}}.

rollback(State) ->
	{ok, State#state{agent_cache=dict:new()}}.

%% Internal
find_agent(Summary) ->
	case proplists:get_value(oncall, Summary) of
		{_, [{Ag, _}|_]} ->
			Ag;
		_ ->
			undefined
	end.

find_queue(Summary) ->
	case proplists:get_value(inqueue, Summary) of
		{_, [{Q, _}|_]} ->
			Q;
		_ ->
			undefined
	end.

get_profile(Ag) when is_list(Ag) ->
	case catch agent_auth:get_agent_by_login(Ag) of
		{ok, #agent_auth{profile=P}} -> P;
		_ -> undefined
	end;
get_profile(_) -> undefined.


as_erl_now(S) when is_integer(S) ->
	{S div 1000000, S rem 1000000, 0};
as_erl_now(_) ->
	null.

fx(null) -> null;
fx(undefined) -> null;
fx(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
fx(Othr) -> Othr.

get_clients(Login, Cache) ->
	{Ag, Cache1} = get_agent(Login, Cache),
	Clients = case Ag of
		#agent_auth{skills=Skills} when is_list(Skills) ->
			[Cl || {'_brand', Cl} <- Skills];
		_ ->
			[]
	end,
	{Clients, Cache1}.

get_agent(Login, Cache) ->
	case dict:find(Login, Cache) of
		{ok, Ag} ->
			{Ag, Cache};
		_ ->
			E = case catch agent_auth:get_agent_by_login(Login) of
				{ok, E1} -> E1;
				_ -> undefined
			end,
			{E, dict:store(Login, E, Cache)}
	end.

get_start([]) ->
	undefined;
get_start([#cdr_raw{transaction=cdrinit}|T]) ->
	get_start(T);
get_start([#cdr_raw{start=Ts}|_]) ->
	Ts.

get_end(Raws) ->
	get_end1(lists:reverse(Raws)).

%% reversed
get_end1([]) ->
	undefined;
get_end1([#cdr_raw{transaction=cdrend}|T]) ->
	get_end1(T);
get_end1([#cdr_raw{start=Ts}|_]) ->
	Ts.


get_call_end(Raws) ->
	get_call_end1(lists:reverse(Raws)).

%% reversed
get_call_end1([]) ->
	undefined;
get_call_end1([#cdr_raw{transaction=oncall, ended=Ts}|_]) ->
	Ts;
get_call_end1([_|T]) ->
	get_call_end1(T).


try_ensure_ndxs() ->
	K = mongoapi:new(spx, <<"openacd">>),
	try
		K:ensureIndex(<<"cdr">>, [{<<"call_end">>, -1}, {<<"agent">>, 1}]),
		K:ensureIndex(<<"asl">>, [{<<"end">>, -1}, {<<"agent">>, 1}])
	catch
		_:Err ->
			lager:warning("Failed to ensure index: ~p", [Err])
	end.

try_insert(Tbl, Entry) ->
	K = mongoapi:new(spx, <<"openacd">>),
	case catch K:insert(Tbl, Entry) of
		ok -> ok;
		Other -> lager:warning("Not able to insert to mongo records due to ~p: ~p", [Other, Entry])
	end.

