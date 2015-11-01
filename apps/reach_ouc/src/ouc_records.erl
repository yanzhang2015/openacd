-module(ouc_records).
-export([load/0, get_cdrs_by_agent/1, get_asls_by_agent/1]).

-include_lib("erlmongo/include/erlmongo.hrl").
-include("ouc_rstat.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(MILLI, 1000).
-define(MICRO, 1000000).

% API

-spec load() -> {ok, [#cpx_cdr{}]}.
load() ->
	{ok, Conf} = ouc_rstat_conf:new([]),
	Db = ouc_db:get_db(openacd),
	{ok, CEntries} = Db:findOpt(<<"cdr">>, #search{criteria = []}, []),
	{ok, AEntries} = Db:findOpt(<<"asl">>, #search{criteria = []}, []),
	Cdrs = [cdr_to_record(E) || E <- CEntries],
	Asls = [asl_to_record(E) || E <- AEntries],
	{ok, {
		[process_cdrs(Cdr, Conf) || Cdr <- Cdrs],
		[process_asls(Asl, Conf) || Asl <- Asls]
	}}.

-spec get_cdrs_by_agent(Agent :: list()) -> {ok, [#cpx_cdr{}]}.
get_cdrs_by_agent(Agent) ->
	Db = ouc_db:get_db(openacd),
	{ok, Cdrs} = Db:findOpt(<<"cdr">>, #search{criteria = [{<<"agent">>, list_to_binary(Agent)}]}, []),
	{ok, [cdr_to_record(Cdr) || Cdr <- Cdrs]}.

get_asls_by_agent(Agent) ->
	Db = ouc_db:get_db(openacd),
	{ok, Asls} = Db:findOpt(<<"asl">>, #search{criteria = [{<<"agent">>, list_to_binary(Agent)}]}, []),
	[asl_to_record(Asl) || Asl <- Asls].

% Internal functions

cdr_to_record(Entry) ->
	CallId = undef_or_list(ej:get({"callid"}, Entry)),
	Agent = undef_or_list(ej:get({"agent"}, Entry)),
	Client = undef_or_list(ej:get({"client"}, Entry)),
	Profile = undef_or_list(ej:get({"profile"}, Entry)),
	Queue = undef_or_list(ej:get({"queue"}, Entry)),
	{array, TransactionsJson} = ej:get({"transactions"}, Entry),
	Transactions = [{binary_to_atom(State, utf8), Ts} || [{<<"state">>, State}, {<<"ts">>, Ts}] <- TransactionsJson],
	#cpx_cdr{callid=CallId, queue=Queue, client=Client, profile=Profile, agent=Agent, transactions=Transactions}.

asl_to_record(Entry) ->
	Agent = undef_or_list(ej:get({"agent"}, Entry)),
	Profile = undef_or_list(ej:get({"profile"}, Entry)),
	{array, ClientsBin} = ej:get({"clients"}, Entry),
	Clients = [undef_or_list(C) || C <- ClientsBin],
	
	State = undef_or_atom(ej:get({"state"}, Entry)),
	OldState = undef_or_atom(ej:get({"old_state"}, Entry)),
	Start = ej:get({"start"}, Entry),
	End = ej:get({"end"}, Entry),
	
	#cpx_asl{agent=Agent, profile=Profile, clients=Clients, state=State, old_state=OldState, start_ts=Start, end_ts=End}.

process_cdrs(Cdr, Conf) ->
	Client = Cdr#cpx_cdr.client,
	Queue = Cdr#cpx_cdr.queue,
	Profile = Cdr#cpx_cdr.profile,

	ProfileNdx = [{profile, Profile}],
	ClientNdx = [{client, Client}],
	QueueNdx = [{queue, Queue}],

	MnIndxs = [ClientNdx, ProfileNdx, QueueNdx],
	MnStore = ouc_rstat_store_mnesia:get_store(),

	case get_ans_dur(Cdr) of
		ignore ->
			[];
		{StartSec, EndSec, AnsDurationSec} ->
			ouc_rstat_store:log_call(MnStore, Conf, StartSec, EndSec, AnsDurationSec, MnIndxs),
			Cdr
	end.

get_ans_dur(Cdr) ->
	Transactions = Cdr#cpx_cdr.transactions,
	RingingTime = proplists:get_value(ringing, Transactions),
	OncallTime = proplists:get_value(oncall, Transactions),
	EndTime = proplists:get_value(wrapup, Transactions),

	case lists:member(undefined, [RingingTime, OncallTime, EndTime]) of
		false ->
			StartSec = util:now_ms(OncallTime) div ?MILLI,
			EndSec = util:now_ms(EndTime) div ?MILLI,
			AnsDuration = timer:now_diff(OncallTime, RingingTime),
			AnsDurationSec = AnsDuration div ?MICRO,

			{StartSec, EndSec, AnsDurationSec};
		_ ->
			ignore
	end.

process_asls(#cpx_asl{old_state=idle, state=AgentSt, start_ts=StartTime, end_ts=EndTime} = Asl, Conf)
		when AgentSt =:= released orelse AgentSt =:= logout ->
	ClientNdxs = [[{client, C}] || C <- Asl#cpx_asl.clients],
	ProfileNdx = [{profile, Asl#cpx_asl.profile}],
	Indices = [ProfileNdx | ClientNdxs],
	
	MnStore = ouc_rstat_store_mnesia:get_store(),
	
	{StartSec, EndSec} = get_avail_dur(StartTime, EndTime),
	ouc_rstat_store:log_availability(MnStore, Conf, StartSec, EndSec, Indices),

	Asl;
process_asls(Asl, _) ->
	Asl.

get_avail_dur(StartTime, EndTime) ->
	StartSec = util:now_ms(StartTime) div ?MILLI,
	EndSec = util:now_ms(EndTime) div ?MILLI,
	{StartSec, EndSec}.

undef_or_list(Bin) when is_binary(Bin) ->
	binary_to_list(Bin);
undef_or_list(_) ->
	undefined.

undef_or_atom(Bin) when is_binary(Bin) ->
	binary_to_atom(Bin, utf8);
undef_or_atom(_) ->
	undefined.

% Eunit tests

-ifdef(TEST).

process_cdrs_test_() ->
	CdrEntry = [{<<"_id">>,{oid,<<"oid">>}},
				{<<"callid">>,<<"abcdef">>},
				{<<"agent">>,<<"1000">>},
				{<<"queue">>,<<"default_queue">>},
				{<<"client">>,<<"Client 1">>},
				{<<"profile">>,<<"Default">>},
				{<<"transactions">>,
					{array,[[{<<"state">>,<<"inivr">>},{<<"ts">>,{1361,344291,0}}],
						[{<<"state">>,<<"ringing">>},{<<"ts">>,{1361,344292,0}}],
						[{<<"state">>,<<"oncall">>},{<<"ts">>,{1361,344294,0}}],
						[{<<"state">>,<<"wrapup">>},{<<"ts">>,{1361,344298,0}}],
						[{<<"state">>,<<"terminate">>},{<<"ts">>,{1361,344298,0}}]]}}],
	CdrRec = #cpx_cdr{callid = "abcdef",agent = "1000",
				profile = "Default", queue = "default_queue", client = "Client 1",
				transactions = [{inivr, {1361,344291,0}},
					{ringing, {1361,344292,0}},
					{oncall, {1361,344294,0}},
					{wrapup, {1361,344298,0}},
					{terminate, {1361,344298,0}}]},
	{setup, fun() ->
		meck:new(ouc_rstat_store),
		meck:expect(ouc_rstat_store, log_call, 6, ok),

		meck:new(mongoapi),
		meck:expect(mongoapi, new, fun(N, Db) -> {mongoapi, N, Db} end),
		meck:expect(mongoapi, findOpt, fun(<<"cdr">>,_,_,_) -> {ok, [CdrEntry]}; (_,_,_,_) -> {ok, []} end)
	end, fun(_) ->
		meck:unload()
	end, [{"log cdrs", fun() ->
		{ok, Conf} = ouc_rstat_conf:new([]),
		MnStore = ouc_rstat_store_mnesia:get_store(),

		?assertEqual({ok, {[CdrRec], []}}, load()),
		?assert(meck:called(ouc_rstat_store, log_call, [MnStore, Conf, 1361344294, 1361344298, 2,
			[[{client,"Client 1"}], [{profile,"Default"}], [{queue,"default_queue"}]]]))
	end}, {"get cdrs by agent", fun() ->
		?assertEqual({ok, [CdrRec]}, get_cdrs_by_agent("1000"))
	end}]}.

-endif.
