-record(inivr_state, {}).

-record(inqueue_state, {
	queue_mon :: 'undefined' | reference(),
	queue_pid :: {string(), 'undefined' | pid()},
	cook :: pid(),
	cook_mon :: reference(),
	outband_ring_pid :: 'undefined' | pid(),
	sent_to_voicemail = false :: boolean()
}).

-record(inqueue_ringing_state, {
	queue_mon :: 'undefined' | reference(),
	queue_pid :: {string(), 'undefined' | pid()},
	cook :: pid(),
	cook_mon :: reference(),
	ring_mon :: 'undefined' | reference(),
	ring_pid :: {string(), pid()},
	ringout :: reference(),
	outband_ring_pid :: 'undefined' | pid(),
	voicemail_on_ring_failure = false :: boolean()
}).

-record(oncall_state, {
	oncall_mon :: 'undefined' | reference(),
	oncall_pid :: {string(), pid()}
}).

-record(oncall_ringing_state, {
	oncall_mon :: 'undefined' | reference(),
	oncall_pid :: {string(), pid()},
	ring_mon :: reference(),
	ring_pid :: {string(), pid()},
	ringout :: reference(),
	outband_ring_pid :: 'undefined' | pid()
}).

%% it is up to the media to maintain a list of held medias.
-record(warm_transfer_hold_state, {
	oncall_mon :: reference(),
	oncall_pid :: {string(), pid()},
	held_refs = [] :: [{any(), any()}],
	merged_refs = [] :: [{any(), any()}],
	caller_ref :: any()
}).

%% it is up to the media to maintain a list of held medias
-record(warm_transfer_3rd_party_state, {
	oncall_mon :: reference(),
	oncall_pid :: {string(), pid()},
	held_refs = [] :: [{any(), any()}],
	merged_refs = [] :: [{any(), any()}],
	active_ref :: {any(), any()},
	caller_ref :: any()
}).

%% it is up tot he media to maintain a list of help medias.
-record(warm_transfer_merged_state, {
	oncall_mon :: reference(),
	oncall_pid :: {string(), pid()},
	merged_refs = [] :: [{any(), any()}],
	caller_ref :: any()
}).

-record(wrapup_state, {
	transfer_state :: 'undefined' | #warm_transfer_merged_state{}
}).

-record(cpx_gen_media_prop, {
	state,
	queue :: string(),
	call :: #call{},
	client,
	state_changes = [] :: [{atom(), tuple()}],
	agent_login :: string(),
	agent_profile :: string()
}).

-record(cpx_gen_media_init, {
	pid :: pid(),
	now :: tuple(),
	prop :: #cpx_gen_media_prop{}
}).

-record(cpx_gen_media_update, {
	pid :: pid(),
	now :: tuple(),
	state :: atom(),
	old_state :: atom(),
	prop :: #cpx_gen_media_prop{}
}).

-record(conference_leg, {
	id :: string(),
	original_id :: string(),
	pid :: pid(),
	callback :: atom() | undefined,
	request :: {agent, string()} | {queue, string()} | {outband, string()},
	agent :: {string(), string() | undefined, string() | undefined} | undefined,
	state :: atom(),
	fs_uuid :: undefined | string()
}).

