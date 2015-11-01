-define(DEFAULT_PORT, 8936).
-define(CKNAME, <<"OUCX">>).
-define(JASPER_CKNAME, <<"JSESSIONID">>).
-define(JASPER_CKPATH, <<"/jasperserver">>).

-record(session, {
	username,
	security_level,
	last_login,
	last_activity,
	logout_code,
	options = []
}).
-record(active_channel, {
	type,
	state,
	callid,
	callerid,
	channelid,
	client,
	state_changes
}).
-record(online_agent, {
	username,
	profile,
	login_time,
	first_name,
	last_name,
	job_title,
	location,
	skills,
	avatar,
	state,
    reach_node,
	active_channels = [] :: [#active_channel{}]
}).


-type profile_count() ::{
	Released::non_neg_integer(),
	Available::non_neg_integer()
}.

-record(profile_counts, {
	total = 0 :: non_neg_integer(),
	released = 0 :: non_neg_integer(),
	idle = 0 :: non_neg_integer(),
	ringing = 0 :: non_neg_integer(),
	oncall = 0 :: non_neg_integer(),
	wrapup = 0 :: non_neg_integer()
}).

-type client_count() ::{
	Released::non_neg_integer(),
	Available::non_neg_integer()
}.

-record(client_counts, {
	client :: string(),
	idle = {0, 0} :: client_count(),
	ringing = {0, 0} :: client_count(),
	oncall = {0, 0} :: client_count(),
	wrapup = {0, 0} :: client_count()
}).

-record(queue_count, {
	queue :: string(),
	calls_queued = 0 :: non_neg_integer(),
	calls_connected = 0:: non_neg_integer()
}).

-record(qcall, {
	id :: string(),
	time_queued :: non_neg_integer(),
	dnis :: string(),
	line :: string(),
	queue :: string(),
    reach_node :: binary() | tuple(),
	total_agent_count :: non_neg_integer(),
	available_agent_count :: non_neg_integer(),
	idle_agent_count :: non_neg_integer(),
	skills = [] :: [atom() | tuple()],
	callerid :: {string(), string()},
	client :: string(),
	type :: atom(),
	source_module :: atom()
}).

%% Permissions

%% Specify what an agent is allowed to see / do in Reach
-define(PERM_ADV_LOGIN_KEY, <<"advlg">>).
-define(PERM_REPORTS_TAB_KEY, <<"rptab">>).
-define(PERM_CALL_REC_KEY, {<<"wdg">>, <<"CallRecording">>}).