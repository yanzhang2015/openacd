define(function () {
  // openacd extension
  // additional oa functions
  // assume this is the oa object
  var openACDSupervisor = {};

  openACDSupervisor.subscribeAgentProfiles = function (callback, context) {
    this.callApi('ouc_sup.subscribe_agent_profiles', [], function (o) {
      if ($.isFunction(callback)) {
        callback.call(context, o.result.profiles);
      }
    }, null, context);
  };

  openACDSupervisor.subscribeAgents = function (callback, context) {
    this.callApi('ouc_sup.subscribe_agents', [], function (o) {
      if ($.isFunction(callback)) {
        callback.call(context, o.result.agents);
      }
    }, null, context);
  };

  openACDSupervisor.setAgentAvailable = function (agentId) {
    this.callApi('ouc_sup.set_agent_state', [agentId, 'available']);
  };

  openACDSupervisor.setAgentReleased = function (agentId, releaseCode) {
    if (!releaseCode) {
      this.callApi('ouc_sup.set_agent_state', [agentId, 'released']);
    }
    else {
      this.callApi('ouc_sup.set_agent_state', [agentId, {'released': releaseCode}]);
    }
  };

  openACDSupervisor.kickAgent = function (agentId) {
    this.callApi('ouc_sup.kick_agent', [agentId]);
  };

  openACDSupervisor.subscribeQueues = function (callback, context) {
    this.callApi('ouc_sup.subscribe_queues', [], function (o) {
      if ($.isFunction(callback)) {
        callback.call(context, o.result.queues);
      }
    }, null, context);
  };

  openACDSupervisor.subscribeQueuedCalls = function (callback, context) {
    this.callApi('ouc_sup.subscribe_queued_calls', [], function (o) {
      if ($.isFunction(callback)) {
        callback.call(context, o.result.queued_calls);
      }
    }, null, context);
  };

  openACDSupervisor.barge = function (channelid, agent, toForceRelease, callback, context) {
    if (this._get('isProcessing')) return;
    if (toForceRelease) this.callApi('go_released');

    this.callApi('ouc_sup.barge_call', [agent, channelid], function (o) {
      if ($.isFunction(callback)) {
        this._set('agentBargePending', agent);
        o.type = 'barge_call';
        callback.call(context, o);
      }
    }, null, this);
  };

  openACDSupervisor.monitor = function (channelid, agent, toForceRelease, callback, context) {
    if (this._get('isProcessing')) return;
    if (toForceRelease) this.callApi('go_released');

    this.callApi('ouc_sup.monitor_call', [agent, channelid], function (o) {
      if ($.isFunction(callback)) {
        this._set('agentMonitorPending', agent);
        o.type = 'monitor_call';
        callback.call(context, o);
      }
    }, null, this);
  };

  openACDSupervisor.getRecordings = function (filterOpts, sortOpts, entryStart, entryEnd, callback, context) {
    if (!entryStart || !entryEnd) return;
    this.callApi('ouc_sup.get_recordings', [filterOpts, sortOpts, entryStart, entryEnd], function (o) {
      if (_.isFunction(callback)) {
        callback.call(context, o.result);
      }
    }, null, context);
  };

  // called after openacd object is extended
  // not added to openacd object
  openACDSupervisor.initialize = function () {
    this._set('agentBargePending', null);
    this._set('agentBargeOn', null);
    this._set('agentMonitorPending', null);
    this._set('agentMonitorOn', null);

    this.events.RECEIVEDPROFILECOUNT = 'received_profile_count';
    this.events.UPDATEDPROFILERSTAT = 'updated_profile_rstat';
    this.events.UPDATEDAGENTS = 'updated_received_agents';
    this.events.RECEIVEDQUEUECOUNT = 'received_queue_count';
    this.events.UPDATEDQUEUEDCALLS = 'updated_queued_calls';
    this.events.UPDATEDQUEUERSTAT = 'updated_queue_rstat';
    this.events.UPDATEDAGENTRSTAT = 'updated_agent_rstat';
    this.events.RECEIVEDCALLMONITORRESULT = 'received_call_monitor_result';
    this.events.RECEIVEDCALLMONITORENDED = 'received_call_monitor_ended';
    this.events.RECEIVEDCALLBARGERESULT = 'received_call_barge_result';
    this.events.RECEIVEDCALLBARGEENDED = 'received_call_barge_ended';

    this.addMessageHandler(function (d) {
      if (d.event === 'call_barge_ended') return true;

      return false;
    }, function (d) {
      this._set('agentBargeOn', null);
      amplify.publish(this.events.RECEIVEDCALLBARGEENDED, d);
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'call_barge_result') return true;

      return false;
    }, function (d) {
      this._set('agentBargePending', null);
      this._set('agentBargeOn', d.data.agent);
      amplify.publish(this.events.RECEIVEDCALLBARGERESULT, d);
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'call_monitor_ended') return true;

      return false;
    }, function (d) {
      this._set('agentMonitorOn', null);
      amplify.publish(this.events.RECEIVEDCALLMONITORENDED, d);
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'call_monitor_result') return true;

      return false;
    }, function (d) {
      this._set('agentMonitorPending', null);
      this._set('agentMonitorOn', d.data.agent);
      amplify.publish(this.events.RECEIVEDCALLMONITORRESULT, d);
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'profile_count') return true;

      return false;
    }, function (d) {
      amplify.publish(this.events.RECEIVEDPROFILECOUNT, _.pick(d, 'data'));
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'agents_update') return true;

      return false;
    }, function (d) {
      amplify.publish(this.events.UPDATEDAGENTS, _.pick(d, 'data'));
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'queue_count') return true;

      return false;
    }, function (d) {
      amplify.publish(this.events.RECEIVEDQUEUECOUNT, _.pick(d, 'data'));
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'profile_rstat_update') return true;

      return false;
    }, function (d) {
      amplify.publish(this.events.UPDATEDPROFILERSTAT, _.pick(d, 'data'));
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'queued_calls_update') return true;

      return false;
    }, function (d) {
      amplify.publish(this.events.UPDATEDQUEUEDCALLS, _.pick(d, 'data'));
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'queue_rstat_update') return true;

      return false;
    }, function (d) {
      amplify.publish(this.events.UPDATEDQUEUERSTAT, _.pick(d, 'data'));
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'agent_rstat_update') return true;

      return false;
    }, function (d) {
      amplify.publish(this.events.UPDATEDAGENTRSTAT, _.pick(d, 'data'));
    });
  };

  return openACDSupervisor;
});