var start_time = new Date(2013, 6, 24, 6, 30, 0);
var end_time = new Date(2014, 6, 26, 6, 45, 0);

map = function () {
	var inivr_ts = null;
	var start_ts = null;
	var end_ts = null;
	var line = null;
	var node = null;
	var origination = null;
	var origination_type = null;
	var client = null;
	var disposition = null;
	var disposition_auto = false;
	var caller_id = null;
	var caller_ani = null;
	var f_ring = 0;
	var ring_count = 0;
	var first_ring_ts = null;
	var last_ring_ts = null;
	var media_id = null;
	var queue_name = null;
	var queue_group = null;
	var f_answer = 0;
	var answer_ts = null;
	var agent_login = null;
	var agent_profile = null;
	var new_call_segment = 0;
	var final_call_segment = 0;
	var from_transfer = 0;
	var to_transfer = 0;
	var transfer_to = null;
	var next_segment = null;
	var f_abandon = 0;
	var ended_by = null;
	var call_segment_ended = 0;
	var wait_duration = 0;
	var oncall_duration = 0;
	switch(this.event_finished){
		case 'inivr' :
			if(this.from != null){
				start_ts = this.from;
				inivr_ts = this.from;
			};
		case 'inqueue':
			if(this.from != null){
				start_ts = this.from;
			};
			if(this.queue != null){
				queue_name = this.queue;
			};
			if(this.queue_group != null){
				queue_group = this.queue_group;
			}
			wait_duration = (this.to - this.from)/1000;
			if(this.line != null){
				line = this.line;
			}
			if(this.event_starting == 'hangup'){ f_abandon = 1;};
			if(this.client_label != null){
				client = this.client_label;
			}
			if(this.caller_id != null){
				caller_id = this.caller_id;
			}
			if(this.caller_ani != null){
				caller_ani = this.caller_ani;
			}
			if(this.disposition != null){
				disposition = this.disposition;
			}
			if(this.media_id != null){
				media_id = this.call_id;
			}
			if(this.node != null){
				node = this.node;
			}
			break;
		case 'ringing':
			f_ring = 1;
			ring_count = 1;
			if(this.from != null){
				first_ring_ts = this.from;
			};
			if(this.to != null){
				last_ring_ts = this.from;
			};
			if(this.event_starting == 'hangup'){ f_abandon = 1;};
			if(this.agent_login != null){
				agent_login = this.agent_login;
			}
			if(this.agent_profile != null){
				agent_profile = this.agent_profile;
			}
			wait_duration = (this.to - this.from)/1000 ;
			break;
		case 'oncall' :
			f_answer = 1;
			if(this.from != null){
				answer_ts = this.from;
			}
			if(this.event_starting == 'hangup'){
				oncall_duration = (this.to - this.from)/1000;
			};
			break;
		case 'voicemail' :
			disposition = 'voicemail';
			disposition_auto = true;
			break;
		case 'hangup' :
			if(this.from != null){
				end_ts = this.from;
			};
			switch(this.ended_by){
				case "agent":
					ended_by = this.agent_login;
					break;
				case "caller":
					ended_by = this.caller_ani;
					break;
				default :
					ended_by = this.ended_by;
			}
			final_call_segment = 1;
			break;
		case 'agent_transfer':
		case 'queue_transfer':
			if(this.event_count != 0){
				to_transfer = 1;
				transfer_to = this.transfer_to;
				disposition = 'transfer';
				disposition_auto = true;
			}
			break;
		case 'agent_conference':
		case 'queue_conference':
			if(this.event_count != 0){
				transfer_to = this.transfer_to;
				disposition = 'conference';
				disposition_auto = false;
			}
			break;
		default :
			if(this.event_finished != null &&
				this.event_finished.media_custom == 'oncall'){
				oncall_duration = (this.to - this.from)/1000;
			};
			break;
	}
	if(this.event_count == 0){
		switch(this.event_starting){
			case 'agent_transfer':
			case 'queue_transfer':
				origination_type = 'transfer';
				origination = this.agent_login;
				next_segment = this.transfer_destination;
				from_transfer = 1;
				break;
			case 'agent_conference':
			case 'queue_conference':
				origination_type = 'conference';
				origination = this.agent_login;
				next_segment = this.transfer_destination;
				break;
		}
	}
	if(this.call_segment_count == 1){
		new_call_segment = 1;
		origination_type = 'line';
		origination = this.line;
	};
	if(this.call_segment_ended == 1){
		call_segment_ended = 1;
	};

	emit({call_id : this.original_id,
		  call_segment_count : this.call_segment_count},
		 {inivr_ts : inivr_ts,
		 	start_ts : start_ts,
			end_ts : end_ts,
			line : line,
			node : node,
			origination : origination,
			origination_type : origination_type,
			client : client,
			disposition : disposition,
			disposition_auto : disposition_auto,
			caller_id : caller_id,
			caller_ani : caller_ani,
			f_ring : f_ring,
			ring_count : ring_count,
			first_ring_ts : first_ring_ts,
			last_ring_ts : last_ring_ts,
			media_id : media_id,
			queue_name : queue_name,
			queue_group : queue_group,
			wait_duration : wait_duration,
			oncall_duration : oncall_duration,
			f_answer : f_answer,
			answer_ts : answer_ts,
			agent_login : agent_login,
			agent_profile : agent_profile,
			new_call_segment : new_call_segment,
			final_call_segment : final_call_segment,
			from_transfer : from_transfer,
			to_transfer : to_transfer,
			transfer_to : transfer_to,
			next_segment : next_segment,
			f_abandon : f_abandon,
			ended_by : ended_by,
			call_segment_ended : call_segment_ended});
};



reduce= function(call_key, calls){
	// csf = call_segment_fact
	var csf = {  inivr_ts : null,
				 start_ts : null,
				 end_ts : null,
				 line : null,
				 node : null,
				 client : null,
				 disposition : null,
				 disposition_auto : null,
				 caller_id : null,
				 caller_ani : null,
				 f_ring : 0,
				 ring_count : 0,
				 first_ring_ts : null,
				 last_ring_ts : null,
				 media_id : null,
				 queue_name : null,
				 queue_group : null,
				 wait_duration : 0,
				 oncall_duration : 0,
				 f_answer : 0,
				 answer_ts : null,
				 agent_login : null,
				 agent_profile : null,
				 new_call_segment : 0,
				 final_call_segment : 0,
				 from_transfer : 0,
				 to_transfer : 0,
				 f_abandon : 0,
				 ended_by : null,
				 call_segment_ended : 0 };





	calls.forEach(function(call){
		for(key in minKeys){
			updateF(minKeys[key], call, csf, min);
		};
		for(key in maxKeys){
			updateF(maxKeys[key], call, csf, max);
		};
		for(key in notNullKeys){
			notNull(notNullKeys[key], call, csf, 1);
		};
		for(key in addKeys){
			updateF(addKeys[key], call, csf, sum);
		};
		for(key in assignSelfKeys){
			notNull(assignSelfKeys[key], call, csf, call[assignSelfKeys[key]])
		}
	});

	return csf;
};

finalize = function(key, reducedVal){
	if(reducedVal.f_answer == 0){
		reducedVal.wait_duration = null;
		reducedVal.oncall_duration = null;
	}
	return reducedVal;
}

//INTERNAL FUNCTIONS
// created own min and max funcs because Math.min returns
// milliseconds when comparing two dates
var min = function(a,b){
	return a <= b ? a : b;
};

var max = function(a,b){
	return a >= b ? a : b;
};

var sum = function(a,b){
	return a + b;
}

var updateF = function(k, call, csf, func) {
	if (call[k] != null){
		if(csf[k] == null){
			csf[k] = call[k];
		}
		else{
			csf[k] = func(csf[k], call[k]);
		}
	}
};

var notNull = function(k, call, csf, val){
	if(call[k]){
		csf[k] = val;
	}
}

var minKeys = ['start_ts', 'first_ring_ts'];
var maxKeys = ['end_ts', 'last_ring_ts'];
var addKeys = ['ring_count',
			   'wait_duration',
			   'oncall_duration'];
var notNullKeys = ['f_ring',
				   'f_answer',
				   'f_abandon',
			   	   'call_segment_ended',
				   'queue_name',
				   'queue_group',
				   'new_call_segment',
				   'final_call_segment'];
var assignSelfKeys = ['inivr_ts',
					  'line',
					  'node',
					  'origination',
					  'origination_type',
					  'client',
					  'disposition',
					  'disposition_auto',
					  'caller_id',
					  'caller_ani',
					  'media_id',
					  'queue_name',
					  'queue_group',
					  'answer_ts',
					  'agent_login',
					  'agent_profile',
					  'from_transfer',
					  'to_transfer',
					  'transfer_to',
					  'next_segment',
					  'ended_by'];

db.events.mapReduce(map, reduce, {out: {reduce : 'active_call_segments'},
								finalize : finalize,
								query : {call_segment_count :
											{ $ne : 0},
										 to :
											{ $gt : start_time,
											  $lte : end_time}},
								scope : { min : min,
										  max : max,
										  sum : sum,
										  updateF : updateF,
										  notNull : notNull,
										  minKeys : minKeys,
										  maxKeys : maxKeys,
										  addKeys : addKeys,
										  notNullKeys : notNullKeys,
										  assignSelfKeys : assignSelfKeys
										}
							   });




var call_segment_ended_list =
	db.active_call_segments.find({'value.call_segment_ended' : 1})
								.toArray();

db.call_segment_facts.insert(call_segment_ended_list);

db.active_call_segments.remove({'value.call_segment_ended' : 1});