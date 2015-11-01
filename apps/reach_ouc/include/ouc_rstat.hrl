-type instant() :: non_neg_integer().
-type duration() :: pos_integer().
-type interval() :: {instant(), instant()}.
-type datetime() :: calendar:datetime().

-type timelen(Qlf) :: {Qlf, pos_integer()}.

-type period() :: timelen(month) | timelen(year).
-type duration_exp() :: timelen(hour) | timelen(minute) | timelen(second).

-type static_conf() :: {static, Name::any(), Len::(period() | duration_exp()), instant()}.
-type dynamic_conf() :: {dynamic, Name::any(), Coverage::duration_exp(), UpdateFreq::duration_exp(), instant()}.
-type conf() :: static_conf() | dynamic_conf().
-type confs() :: [conf()].

-type pod() :: period() | duration().

-type update_rule() :: {pod(), instant()}.

-type rstat_prop() :: {total | max, ended | split, atom()}.

-type rstat_index() :: [any()].
-type rstat_cindex() :: rstat_index() | {any, [rstat_index()]}.

-type rstat_key() :: {interval(), rstat_prop(), [rstat_index()]}.

-type conf_err() :: invalid_conf | invalid_coverage | invalid_pivot | invalid_update_freq.

% calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).
-define(UNIX_EPOCH_GSEC, 62167219200).

-define(DEFAULT_PIVOT, {{{2013, 1, 1}, {0, 0, 0}}, local}).

-define(DEFAULT_CONF, [
	{dynamic, last_15m, {minute, 15}, 5},
	{dynamic, last_30m, {minute, 30}, 5},
	{dynamic, last_hr, {hour, 1}, 5},
	{static, today, {day, 1}},
	{static, this_week, {day, 7}},
	{static, this_month, {month, 1}}
]).

-record(rstat_props, {
	total_call_count=0::non_neg_integer(),
	total_call_duration=0::non_neg_integer(),
	total_answer_duration=0::non_neg_integer(),
	max_call_duration=0::non_neg_integer(),
	total_avail_duration=0::non_neg_integer()
}).

-type rule_name() :: atom().

-type client_name() :: string().

-type cpx_transaction() :: {State::atom(), Ts::tuple()}.

-record(cpx_cdr, {
	callid :: list(),
	agent :: list(),
	profile :: list(),
	queue :: list(),
	client :: list(),
	transactions :: [cpx_transaction()]
}).

-record(cpx_asl, {
	agent,
	profile,
	clients,
	state,
	old_state,
	start_ts,
	end_ts
}).

%% Properties

% Basic
-define(PROP_TE_CALL_COUNT, {total, ended, call_count}).
-define(PROP_TE_ABANDON_COUNT, {total, ended, abandon_count}).
-define(PROP_TE_ABANDON_DURATION, {total, ended, abandon_duration}).
-define(PROP_TE_CALL_DURATION, {total, ended, call_duration}).
-define(PROP_TE_RING_DURATION, {total, ended, ring_duration}).
-define(PROP_TE_WAIT_DURATION, {total, ended, wait_duration}).

-define(PROP_ME_CALL_DURATION, {max, ended, call_duration}).
-define(PROP_ME_WAIT_DURATION, {max, ended, wait_duration}).

-define(PROP_TS_AVAIL_DURATION, {total, split, avail_duration}).
-define(PROP_TS_CALL_DURATION, {total, split, call_duration}).

-define(PROP_TS_RELEASED_DURATION, {total, split, released_duration}).
-define(PROP_TS_IDLE_DURATION, {total, split, idle_duration}).
-define(PROP_TS_RINGING_DURATION, {total, split, ringing_duration}).
-define(PROP_TS_ONCALL_DURATION, {total, split, oncall_duration}).
-define(PROP_TS_WRAPUP_DURATION, {total, split, wrapup_duration}).

% Derived
-define(PROP_AvE_CALL_DURATION, {average, ?PROP_TE_CALL_DURATION, ?PROP_TE_CALL_COUNT}).
-define(PROP_AvE_ABANDON_DURATION, {average, ?PROP_TE_ABANDON_DURATION, ?PROP_TE_ABANDON_COUNT}).
-define(PROP_AvE_RING_DURATION, {average, ?PROP_TE_RING_DURATION, ?PROP_TE_CALL_COUNT}).
-define(PROP_AvE_WAIT_DURATION, {average, ?PROP_TE_WAIT_DURATION, ?PROP_TE_CALL_COUNT}).
-define(PROP_PcS_OCCUPANCY, {derived, occupancy}).
