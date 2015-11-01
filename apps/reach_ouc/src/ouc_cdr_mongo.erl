-module(ouc_cdr_mongo).
-behaviour(gen_event).

%% API
-export([
	register/0,
	get_log_mods/0
]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
	handle_info/2, code_change/3, terminate/2]).

-import(cpx_json_util, [b2l/1]).

-include_lib("reach_core/include/agent.hrl").
-include_lib("reach_core/include/call.hrl").
-include_lib("reach_core/include/queue.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SERVER, ?MODULE).
-define(VERSION, 1).

-record(state, {
	id,
	client,
	last_agent,
	last_queue,
	last_event = undefined,
	last_call,
	last_event_start,
	state_changes = [],
	mon :: reference(),
	db_data = [],
	event_count = 1,
	call_segment,
	call_ended,
	wrapup_started = false
}).

%% API

register() ->
	cpx_hooks:set_hook(ouc_cdr_mongo, get_log_mods, ?MODULE, get_log_mods, [], 100).
get_log_mods() ->
	{ok, [?MODULE]}.

%% gen_event callbacks

init([Call]) ->
	CallId = Call#call.id,
	CallSegment = Call#call.call_segment,
	Client = case Call#call.client of
		#client{label=L} -> L;
		_ -> undefined
	end,

	Mon = erlang:monitor(process, Call#call.source),

	{ok, #state{id = CallId, call_segment = CallSegment, client = Client,
				mon = Mon}}.

handle_event({Event = 'inivr', _Call=#call{id = CallId, call_segment = CallSegment},
		Time, _Data},
		State=#state{id = CallId, call_segment = CallSegment}) ->

	{ok, State#state{last_event = Event,
					 last_event_start = Time}};

handle_event({Event = 'inqueue', Call=#call{id = CallId, call_segment = CallSegment},
		Time, _Data},
		State=#state{id = CallId, call_segment = CallSegment}) ->


	CallEntry = get_call_entry(Call),
	ClientEntry = get_client_entry(Call),
	LineEntry = get_line_entry(Call#call.dnis),

	DbEntry = lists:append([CallEntry, ClientEntry, LineEntry]),

	CallSegmentEntry = case check_call_segment_ended(Event, State#state.last_event) of
		true -> [{<<"call_segment_ended">>, fx(1)}];
		_ -> []
	end,

	write_to_db(Event, DbEntry ++ CallSegmentEntry, Time, State),

	MediaIdEntry = get_media_id_entry(Call#call.media_id, Call#call.id),
	QueueEntry = get_queue_entry(Call#call.queue, Call#call.skills),
	NewDbEntry = DbEntry ++ QueueEntry ++ MediaIdEntry,

	NewState = State#state{event_count = State#state.event_count + 1,
						   last_event = Event,
						   last_event_start = Time,
						   db_data = NewDbEntry},

	{ok, NewState};

handle_event({Event = 'ringing', _Call=#call{id = CallId, call_segment = CallSegment},
		Time, Data},
		State=#state{id = CallId, call_segment = CallSegment}) ->

	DbEntry = State#state.db_data,

	write_to_db(Event, DbEntry, Time, State),

	AgentEntry = get_agent_entry(Data),
	NewDbEntry = DbEntry ++ AgentEntry,

	NewState = State#state{event_count = State#state.event_count + 1,
					   last_event = Event,
					   last_event_start = Time,
					   db_data = NewDbEntry},

	{ok, NewState};

handle_event({Event = 'ringout', _Call=#call{id = CallId, call_segment = CallSegment},
		Time, _Data},
		State=#state{id = CallId, call_segment = CallSegment}) ->

	DbEntry = State#state.db_data,

	write_to_db(Event, DbEntry, Time, State),

	NewDbEntry = remove_agent_entry(DbEntry),

	NewState = State#state{event_count = State#state.event_count + 1,
					   last_event = Event,
					   last_event_start = Time,
					   db_data = NewDbEntry},

	{ok, NewState};

handle_event({Event = 'hangup', _Call=#call{id = CallId, call_segment = CallSegment},
		Time, Data},
		State=#state{id = CallId, call_segment = CallSegment}) ->

	DbEntry = State#state.db_data,

	CallSegmentEntry = case check_call_segment_ended(Event, State#state.last_event) of
		true -> [{<<"call_segment_ended">>, fx(1)}];
		_ -> []
	end,

	write_to_db(Event, DbEntry ++ CallSegmentEntry, Time, State),

	HangupEntry = [{<<"ended_by">>, fx(Data)}],
	NewDbEntry = DbEntry ++ HangupEntry,

	NewState = State#state{event_count = State#state.event_count + 1,
					   last_event = Event,
					   last_event_start = Time,
					   db_data = NewDbEntry},

	{ok, NewState};

handle_event({Event = 'agent_transfer', _Call=#call{id = CallId, call_segment = CallSegment},
		Time, Data},
		State=#state{id = CallId, call_segment = CallSegment}) ->

	DbEntry = State#state.db_data,

	NewAgent = proplists:get_value(new_agent, Data),
	NextCallSegment = proplists:get_value(new_call_segment, Data),

	CallSegmentEntry = [{<<"call_segment_ended">>, fx(1)},
						{<<"transfer_destination">>, fx_call_segment(NextCallSegment)}],

	TransferEntry = [{<<"transfer_to">>, fx(NewAgent)}],

	NewDbEntry = DbEntry ++ TransferEntry,

	write_to_db(Event, NewDbEntry ++ CallSegmentEntry, Time, State),

	write_to_db(Event, prep_transfer_call_segment(NewDbEntry, CallSegment),
	Time, State#state{event_count = 0}),

	NewState = State#state{event_count = State#state.event_count + 1,
					   last_event = Event,
					   last_event_start = Time,
					   db_data = NewDbEntry},

	{ok, NewState};

handle_event({Event = 'queue_transfer', _Call=#call{id = CallId, call_segment = CallSegment},
		Time, Data},
		State=#state{id = CallId, call_segment = CallSegment}) ->

	DbEntry = State#state.db_data,

	NewQueue = proplists:get_value(new_queue, Data),
	NextCallSegment = proplists:get_value(new_call_segment, Data),

	CallSegmentEntry = [{<<"call_segment_ended">>, fx(1)},
						{<<"transfer_destination">>, fx_call_segment(NextCallSegment)}],

	TransferEntry = [{<<"transfer_to">>, fx(NewQueue)}],

	NewDbEntry = DbEntry ++ TransferEntry,

	write_to_db(Event, NewDbEntry ++ CallSegmentEntry, Time, State),

	write_to_db(Event, prep_transfer_call_segment(NewDbEntry, CallSegment),
	Time, State#state{event_count = 0}),

	NewState = State#state{event_count = State#state.event_count + 1,
					   last_event = Event,
					   last_event_start = Time,
					   db_data = NewDbEntry},

	{ok, NewState};

handle_event({Event = 'outband_transfer', _Call=#call{id = CallId, call_segment = CallSegment},
		Time, Data},
		State=#state{id = CallId, call_segment = CallSegment}) ->

	lager:info("Transfering call ~p with data ~p", [CallId, Data]),
	DbEntry = State#state.db_data,

	{Destination, Conditions} = case Data of
			{Dest, Cond} -> {Dest, Cond};
			Dest -> {Dest, undefined}
		end,

	{CEntry, Act} = case Conditions of
		undefined -> {[], []};
		_ ->
			ConditionEntry = get_condition_entry(Conditions),
			Action = [{<<"action_taken">>, fx('transfer_outband')}],
			{ConditionEntry, Action}
	end,

	CallSegmentEntry = [{<<"call_segment_ended">>, fx(1)}],

	TransferEntry = [{<<"transfer_to">>, fx(Destination)}],

	NewDbEntry = lists:append([DbEntry, TransferEntry, CEntry, Act]),

	write_to_db(Event, NewDbEntry ++ CallSegmentEntry, Time, State),

	write_to_db(Event, prep_transfer_call_segment(NewDbEntry, CallSegment),
	Time, State#state{event_count = 0}),

	NewState = State#state{event_count = State#state.event_count + 1,
					   last_event = Event,
					   last_event_start = Time,
					   db_data = NewDbEntry},

	{ok, NewState};

handle_event({Event = 'agent_conference', _Call=#call{id = CallId, call_segment = CallSegment},
		Time, Data}, State=#state{id = CallId, call_segment = CallSegment}) ->

	DbEntry = State#state.db_data,

	NewAgent = proplists:get_value(new_agent, Data),
	NextCallSegment = proplists:get_value(new_call_segment, Data),

	CallSegmentEntry = [{<<"call_segment_ended">>, fx(1)},
						{<<"transfer_destination">>, fx_call_segment(NextCallSegment)}],

	TransferEntry = [{<<"conference_to">>, fx(NewAgent)}],

	NewDbEntry = DbEntry ++ TransferEntry,

	write_to_db(Event, NewDbEntry ++ CallSegmentEntry, Time, State),

	write_to_db(Event, prep_conf_call_segment(NewDbEntry, CallSegment),
		Time, State#state{event_count = 0}),

	NewState = State#state{event_count = State#state.event_count + 1,
					   last_event = Event,
					   last_event_start = Time,
					   db_data = NewDbEntry},

	{ok, NewState};

handle_event({Event = 'queue_conference', _Call=#call{id = CallId, call_segment = CallSegment},
		Time, Data},
		State=#state{id = CallId, call_segment = CallSegment}) ->

	DbEntry = State#state.db_data,

	NewQueue = proplists:get_value(new_queue, Data),
	NextCallSegment = proplists:get_value(new_call_segment, Data),

	CallSegmentEntry = [{<<"call_segment_ended">>, fx(1)},
						{<<"transfer_destination">>, fx_call_segment(NextCallSegment)}],

	TransferEntry = [{<<"conference_to">>, fx(NewQueue)}],

	NewDbEntry = DbEntry ++ TransferEntry,

	write_to_db(Event, NewDbEntry ++ CallSegmentEntry, Time, State),

	write_to_db(Event, prep_conf_call_segment(NewDbEntry, CallSegment),
	Time, State#state{event_count = 0}),

	NewState = State#state{event_count = State#state.event_count + 1,
					   last_event = Event,
					   last_event_start = Time,
					   db_data = NewDbEntry},

	{ok, NewState};

handle_event({Event = 'oncall', Call=#call{id = CallId, call_segment = CallSegment},
		Time, _Data},
		State=#state{id = CallId, call_segment = CallSegment}) ->

	CallEntry = get_final_call_entry(Call),
	DbEntry = State#state.db_data ++ CallEntry,

	CallSegmentEntry = case check_call_segment_ended(Event, State#state.last_event) of
		true -> [{<<"call_segment_ended">>, fx(1)}];
		_ -> []
	end,

	write_to_db(Event, DbEntry ++ CallSegmentEntry, Time, State),

	NewState = State#state{event_count = State#state.event_count + 1,
					   last_event = Event,
					   last_event_start = Time,
					   db_data = DbEntry},

	case Event of
		'endwrapup' -> remove_handler;
		'wrapup' -> {ok, NewState#state{wrapup_started = true}};
		_ -> {ok, NewState}
	end;

handle_event({Event = 'remove_skills', _Call=#call{id = CallId, call_segment = CallSegment},
		Time, Data},
		State=#state{id = CallId, call_segment = CallSegment}) ->

	DbEntry = [{<<"call_id">>, fx(CallId)}, {<<"call_segment_count">>, fx_call_segment(CallSegment)}],
	ConditionEntry = get_condition_entry(Data),
	Action = [{<<"action_taken">>, fx('remove_skills')}],

	NewDbEntry = lists:append([DbEntry, ConditionEntry, Action]),

	lager:info("REmove skills DbEntry is ~p", [NewDbEntry]),

	write_to_db(Event, NewDbEntry, Time, State),

	NewState = State#state{event_count = State#state.event_count + 1,
					   last_event = Event,
					   last_event_start = Time},

	{ok, NewState};


handle_event({Event = 'add_skills', _Call=#call{id = CallId, call_segment = CallSegment},
		Time, Data},
		State=#state{id = CallId, call_segment = CallSegment}) ->

	DbEntry = [{<<"call_id">>, fx(CallId)}, {<<"call_segment_count">>, fx_call_segment(CallSegment)}],
	ConditionEntry = get_condition_entry(Data),
	Action = [{<<"action_taken">>, fx('add_skills')}],

	NewDbEntry = lists:append([DbEntry, ConditionEntry, Action]),

	lager:info("Add skills DbEntry is ~p", [NewDbEntry]),

	write_to_db(Event, NewDbEntry, Time, State),

	NewState = State#state{event_count = State#state.event_count + 1,
					   last_event = Event,
					   last_event_start = Time},

	{ok, NewState};

handle_event({Event = 'announce', _Call=#call{id = CallId, call_segment = CallSegment},
		Time, Data},
		State=#state{id = CallId, call_segment = CallSegment}) ->

	DbEntry = [{<<"call_id">>, fx(CallId)}, {<<"call_segment_count">>, fx_call_segment(CallSegment)}],
	ConditionEntry = get_condition_entry(Data),
	Action = [{<<"action_taken">>, fx('announce')}],

	NewDbEntry = lists:append([DbEntry, ConditionEntry, Action]),

	lager:info("Add skills DbEntry is ~p", [NewDbEntry]),

	write_to_db(Event, NewDbEntry, Time, State),

	NewState = State#state{event_count = State#state.event_count + 1,
					   last_event = Event,
					   last_event_start = Time},

	{ok, NewState};

handle_event({Event = 'voicemail', _Call=#call{id = CallId, call_segment = CallSegment},
		Time, {_Queue, Data}},
		State=#state{id = CallId, call_segment = CallSegment}) ->

	DbEntry = [{<<"call_id">>, fx(CallId)}, {<<"call_segment_count">>, fx_call_segment(CallSegment)}],
	ConditionEntry = get_condition_entry(Data),
	Action = [{<<"action_taken">>, fx('voicemail')}],

	NewDbEntry = lists:append([DbEntry, ConditionEntry, Action]),

	lager:info("Add skills DbEntry is ~p", [NewDbEntry]),

	write_to_db(Event, NewDbEntry, Time, State),

	NewState = State#state{event_count = State#state.event_count + 1,
					   last_event = Event,
					   last_event_start = Time},

	{ok, NewState};

handle_event({Event, _Call=#call{id = CallId, call_segment = CallSegment},
		Time, _Data},
		State=#state{id = CallId, call_segment = CallSegment}) ->

	DbEntry = State#state.db_data,

	CallSegmentEntry = case check_call_segment_ended(Event, State#state.last_event) of
		true -> [{<<"call_segment_ended">>, fx(1)}];
		_ -> []
	end,

	write_to_db(Event, DbEntry ++ CallSegmentEntry, Time, State),

	NewState = State#state{event_count = State#state.event_count + 1,
					   last_event = Event,
					   last_event_start = Time},

	case Event of
		'endwrapup' -> remove_handler;
		'wrapup' -> {ok, NewState#state{wrapup_started = true}};
		_ -> {ok, NewState}
	end;

handle_event(_Event, State) ->
	{ok, State}.

handle_call(_Request, State) ->
	Reply = ok,
	{ok, Reply, State}.
handle_info({'DOWN', Mon, process, _Pid, Reason}, State=#state{mon = Mon}) ->
	Time = ouc_time:now(),
	Event = 'stopped',
	% State1 = log_state_change(stop, Time, State, []),
	% mn_log_call(State1, Time),

	DbEntry = State#state.db_data,

	ReasonEntry = [{<<"reason">>, fx(Reason)}],

	write_to_db(Event, ReasonEntry ++ DbEntry, Time, State),

	NewDbEntry = DbEntry ++ [{"call_ended", fx(1)}],

	NewState = State#state{event_count = State#state.event_count + 1,
					   last_event = Event,
					   last_event_start = Time,
					   db_data = NewDbEntry},


	case State#state.wrapup_started of
		true -> {ok, NewState};
		_ -> remove_handler
	end;

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Reason, _State) ->
	% Time = ouc_time:now(),
	% Event = 'terminated',

	% DbEntry = State#state.db_data,

	% NewDbEntry = DbEntry ++ [{"call_ended", fx(1)}],

	% ReasonEntry = [{<<"reason">>, fx(Reason)}],

	% write_to_db(Event, ReasonEntry ++ NewDbEntry, Time, State),

	remove_handler.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal functions

get_call_entry(Call) ->
	[{<<"call_id">>, fx(Call#call.id)},
	{<<"original_id">>, fx(Call#call.original_id)},
	{<<"call_segment_count">>, fx_call_segment(Call#call.call_segment)},
	{<<"call_type">>, fx(Call#call.type)},
	{<<"media_type">>, fx(Call#call.media_type)},
	{<<"direction">>, fx(Call#call.direction)},
	{<<"source_module">>, fx(Call#call.source_module)},
	{<<"original_skills">>, fx(Call#call.skills)},
	{<<"node">>, fx(erlang:node(Call#call.source))}].

get_final_call_entry(Call) ->
	[{<<"final_skills">>, fx(Call#call.skills)}].

get_client_entry(Call) ->
	{CallerId, CallerANI} = get_caller_id(Call#call.callerid),
	Client = Call#call.client,
	[{<<"caller_id">>, fx(CallerId)},
	 {<<"caller_ani">>, fx(CallerANI)},
	  	{<<"client_id">>, fx(Client#client.id)},
		{<<"client_label">>, fx(Client#client.label)}].

get_line_entry(Line) ->
	case get_line_info(Line) of
		{LineName, LineQueue} ->
			[{<<"line">>, fx(Line)},
				{<<"line_name">>, fx(LineName)},
				{<<"line_extension">>, fx(Line)},
				{<<"line_queue">>, fx(LineQueue)}];
		_ -> []
	end.


get_line_info(Line) ->
	Db = ouc_db:get_db(imdb),
	{ok, Props} = Db:findOne(<<"entity">>, [{<<"ent">>, <<"openacdline">>},
						{<<"als.id">>, fx(Line)}]),
	Name = case ej:get({"linm"}, Props, null) of
		null ->
			undefined;
		N ->
			b2l(N)
	end,
	Queue = case ej:get({"qnm"}, Props, null) of
		null ->
			undefined;
		Q ->
			b2l(Q)
	end,
	{Name, Queue}.

get_caller_id(CallerIdTuple) ->
	case CallerIdTuple of
		{X, Y} -> {X, Y};
		_ -> {undefined, undefined}
	end.

write_to_db(CurEvent, Entry, Time, State) ->
	EventEntry = get_event_entry(CurEvent, Time, State),
	VersionEntry = [{<<"vsn">>, ?VERSION}],
	DbEntry = EventEntry ++ Entry ++ VersionEntry,
	Db = ouc_db:get_db(reports),
	Db:save("events", DbEntry).

prep_transfer_call_segment(Entry, CallSegment) ->
	CallSegmentKey = <<"call_segment_count">>,
	ModEntry = Entry -- [{CallSegmentKey, fx_call_segment(CallSegment)}],
	MajorCallSegment = lists:last(CallSegment),
	ModEntry ++ [{CallSegmentKey, fx_call_segment([MajorCallSegment + 1])}].

prep_conf_call_segment(Entry, CallSegment) ->
	CallSegmentKey = <<"call_segment_count">>,
	ModEntry = Entry -- [{CallSegmentKey, fx_call_segment(CallSegment)}],
	ModEntry ++ [{CallSegmentKey, fx_call_segment([1|CallSegment])}].

get_event_entry(CurEvent, Time, State) ->
	[{<<"event_finished">>, fx(State#state.last_event)},
	 {<<"event_count">>, fx(State#state.event_count)},
	 {<<"from">>, as_erl_now(State#state.last_event_start)},
	 {<<"to">>, as_erl_now(Time)},
	 {<<"event_starting">>, fx(CurEvent)}
	].

get_media_id_entry(MediaId, CallId) ->
	FinalVal = case MediaId of
		undefined -> CallId;
		Else -> Else
	end,
	[{<<"media_id">>, fx(FinalVal)}].

get_queue_entry(QueueName, SkillsReq) ->
	{ok, Queue} = call_queue_config:get_queue(QueueName),
	QGroup = Queue#call_queue.group,
	[{<<"queue">>, fx(QueueName)},
	 {<<"queue_group">>, fx(QGroup)},
	 {<<"skills_required">>, fx(SkillsReq)}].

get_condition_entry(Conditions) ->
	[{<<"criteria_matched">>,{array, expand_conditions(Conditions)}}].

expand_conditions([]) ->
	[];
expand_conditions(undefined) ->
	[];
expand_conditions([{Tick, Seconds}|Others]) ->
	[{struct, [{<<"tick">>, fx(Tick)}, {<<"seconds">>, fx(Seconds)}]}]
		++ expand_conditions(Others);
expand_conditions([{Criteria, Condition, Data}|Others]) ->
	[{struct, [{<<"criteria">>, fx(Criteria)}, {<<"condition">>, fx(Condition)}, {<<"data">>, fx(Data)}]}]
		++ expand_conditions(Others).

get_agent_entry([{_, AgentLogin}, {_, AgentPid}]) ->
	AgentInfo = ouc_agent_info:get_agent_info(AgentPid),
	[
		{<<"agent_login">>, fx(AgentLogin)},
		{<<"agent_firstname">>, fx(AgentInfo#agent_info.firstname)},
		{<<"agent_lastname">>, fx(AgentInfo#agent_info.lastname)},
		{<<"agent_inherent_skills">>,
			fx(AgentInfo#agent_info.inherentskills)},
		{<<"agent_profile">>, fx(AgentInfo#agent_info.profile)},
		{<<"agent_profile_skills">>,
			fx(AgentInfo#agent_info.profileskills)},
		{<<"agent_effective_skills">>,
			fx(AgentInfo#agent_info.skills)},
		{<<"agent_node">>,
			fx(erlang:node(AgentPid))}
	].

remove_agent_entry(DbEntry) ->
	AgentEntryKeys = agent_entry_keys(),
	lists:foldl(fun(AgentEntryKey, Acc) -> proplists:delete(AgentEntryKey, Acc) end,
		DbEntry, AgentEntryKeys).

agent_entry_keys() ->
	[
		<<"agent_login">>,
		<<"agent_firstname">>,
		<<"agent_lastname">>,
		<<"agent_inherent_skills">>,
		<<"agent_profile">>,
		<<"agent_profile_skills">>,
		<<"agent_effective_skills">>
	].
% check_new_call_segment('inqueue', PreviousEvent) ->
% 	case PreviousEvent of
% 		'queue_transfer' -> 1;
% 		'oncall' -> 1;
% 		{ 'media_custom', 'oncall'} -> 1;
% 		_ -> 0
% 	end.

check_call_segment_ended(CurrentEvent, PreviousEvent) ->
	case {CurrentEvent, PreviousEvent} of
		{'inqueue', 'ringing'} -> false;
		{'inqueue', 'inivr'} -> false;
		{'inqueue', 'undefined'} -> false;
		{'inqueue', _} -> true;
		{'hangup', _} -> true;
		_ -> false
	end.

% count_event(Event, StateChanges) ->
% 	lists:foldl(
% 		fun({Key, _}, Sum) ->
% 			case Key of
% 				Event -> Sum + 1;
% 				_ -> Sum
% 			end
% 		end,
% 	0, StateChanges).

%% util functions

% merge(Key, OldVal, NewVal) ->
% 	NewVal.

fx_call_segment(CSList) when is_list(CSList) ->
	Rev = lists:reverse(CSList),
	RevString = [erlang:integer_to_list(A) || A <- Rev],
	string:join(RevString, ".");
fx_call_segment(Other) -> Other.

fx(null) -> null;
fx(undefined) -> null;
fx(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
fx({array, List}) ->
	{array, List};
fx(Tuple) when is_tuple(Tuple) ->
	List = tuple_to_list(Tuple),
	FixedList = [fx(A) || A <- List],
	[list_to_tuple(FixedList)];
fx([]) -> [];
fx(List) when is_list(List) ->
	fx_array(List, lists:last(List));
fx(Othr) -> Othr.

fx_array(List, Last) when is_list(Last) ->
	{array, [fx(A) || A <- List]};
fx_array(List, Last) when is_atom(Last) ->
	{array, [fx(A) || A <- List]};
fx_array(List, Last) when is_tuple(Last) ->
	{array, [fx(A) || A <- List]};
fx_array(List, _) ->
	List.

as_erl_now(S) when is_integer(S) ->
	{S div 1000000, S rem 1000000, 0};
as_erl_now(_) ->
	null.
