-module(ouc_rstat_types).

-compile([export_all]).

-include_lib("proper/include/proper.hrl").

rule_name() -> ?LET(X, integer(1, 10),
	list_to_atom([$r|integer_to_list(X)])).

duration() -> pos_integer().
instant() ->
	%% start with 1990/01/01 00:00:00 UTC
	Start = ouc_time:datetime_to_unixts({{1990, 1, 1}, {0, 0, 0}}),
	?LET(X, ?SIZED(Sz, resize(Sz*Sz*Sz*Sz*8000, non_neg_integer())),
		Start + X).

interval() ->
	?LET({X, D}, {instant(), integer(0, 1000000)}, {X, X+D}).

period() ->
	oneof([
		{day, integer(1, 1000)},
		{month, integer(1, 100)}
	]).

duration_ext() ->
	oneof([
		duration(),
		{second, integer(1, 100000000)},
		{minute, integer(1, 1000000)},
		{hour, integer(1, 1000000)}
	]).

period_ext() ->
	oneof([
		period(),
		{year, integer(1, 10)}
	]).

pod_ext() ->
	oneof([duration_ext(), period_ext()]).

pivot() ->
	instant().

raw_cov_update_pair() ->
	%% wow, there's actually a match!
	?SUCHTHAT({C, U}, {duration_ext(), duration_ext()}, begin
		{ok, Cd} = ouc_time:to_duration(C),
		{ok, Ud} = ouc_time:to_duration(U),
		Cd rem Ud =:= 0
	end).

raw_unchecked_static_rule() ->
	N = default(rule_name(), any()),
	C = default(pod_ext(), any()),
	P = default(oneof([pivot(), none]), any()),
	{static, N, C, P}.

raw_unchecked_dynamic_rule() ->
	N = default(rule_name(), any()),
	P = default(oneof([pivot(), none]), any()),
	?LET({C, U},
		oneof([raw_cov_update_pair(), {pod_ext(), pod_ext()}, {any(), any()}]),
		{dynamic, N, C, U, P}).

raw_unchecked_rule() ->
	oneof([raw_unchecked_static_rule(), raw_unchecked_dynamic_rule()]).

raw_dynamic_rule() ->
	?LET({N, U, CDiv, P}, {rule_name(), pos_integer(), pos_integer(), pivot()}, begin
		C = U * CDiv,
		oneof([
			{dynamic, N, C, U, P},
			{dynamic, N, C, U}
		])
	end).

raw_static_rule() ->
	Name = rule_name(),
	Coverage = oneof([duration_ext(), period_ext()]),
	Pivot = pivot(),
	oneof([
		{static, Name, Coverage},
		{static, Name, Coverage, Pivot}
	]).

raw_rule() -> oneof([raw_dynamic_rule(), raw_static_rule()]).

dynamic_rule() ->
	?LET(RR, raw_dynamic_rule(), begin
		{ok, R} = ouc_rstat_rule:from_conf(RR),
		R
	end).

static_rule() ->
	?LET(RR, raw_static_rule(), begin
		{ok, R} = ouc_rstat_rule:from_conf(RR),
		R
	end).

rule() -> oneof([dynamic_rule(), static_rule()]).

%% confs

conf() ->
	?LET(RRs, list(raw_rule()), begin
		{ok, C} = ouc_rstat_conf:new([{rules, RRs}]),
		C
	end).