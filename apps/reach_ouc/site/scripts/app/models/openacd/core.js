define(function () {
  // Model representing the openacd api
  // connect to ViewModels via pub/sub
  var OpenACD = function (config) {
    this.baseUrl = config.wsUrl;
    this.baseUrls = [{
      url: config.wsUrl,
      status: 0
    }];
    this.pingInterval = config.pingInterval;
    this.pongTimeout = config.pongWait + 1;
    this.sleepDuration = config.sleepDuration || 60000;
    this.logPattern = config.logPattern;

    // web socket things
    this.requestId = 0;
    this.ws = null;
    this.callbacks = [];
    this.errCallbacks = [];
    this.messageHandlers = [];
    this.serverTimeSkewMs = 0;
    this.isConnectionReplaced = false;
    this.isConnectionKicked = false;
    this.isSupervisorInit = false;

    this.events = {};
    this.events.WEBSOCKETERROR = 'websocket_error';
    this.events.CONNECTED = 'connected';
    this.events.DISCONNECTED = 'disconnected';
    this.events.FORCEDRELEASE = 'forced_release';
    this.events.UPDATEDCHANNELPLAYBACK = 'updated_channel_playback';
    this.events.PENDINGRELEASESTATE = 'pending_release_state';
    this.events.RECEIVEDRELEASEDSTATE = 'received_release_state';
    this.events.RECEIVEDAVAILABLESTATE = 'received_available_state';
    this.events.RECEIVEDIDLESTATE = 'received_idle_state';
    this.events.RECEIVEDPRECALLSTATE = 'received_precall_state';
    this.events.RECEIVEDINCOMINGSTATE = 'received_incoming_state';
    this.events.RECEIVEDOUTGOINGSTATE = 'received_outgoing_state';
    this.events.RECEIVEDINSESSIONSTATE = 'received_in_session_state';
    this.events.RECEIVEDWRAPUPSTATE = 'received_wrap_up_state';
    this.events.RECEIVEDENDCHANNEL = 'received_end_channel';
    this.events.UPDATEDMYROLLINGSTATS = 'updated_my_rolling_stats';
    this.events.UPDATEDOUTBOUNDCALL = 'updated_outbound_call';
    this.events.UPDATEDTRANSFERAGENTS = 'updated_transfer_agents';
    this.events.UPDATEDCONFERENCE = 'updated_conference';
    this.events.UPDATEDHOLD = 'updated_hold';

    // set message handlers
    this.addMessageHandler(function (d) {
      if (d.event === 'stop' && d.message === 'replaced') return true;

      return false;
    }, function (d) {
      this.isConnectionReplaced = true;
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'stop' && d.message === 'kicked') return true;

      return false;
    }, function (d) {
      this.isConnectionKicked = true;
    });

    this.addMessageHandler(function (d) {
      if (d.command === 'setchannel') return true;

      return false;
    }, function (d) {
      this._set('isProcessing', true);

      switch (d['state']) {
      case 'prering':
        amplify.publish(this.events.RECEIVEDPRECALLSTATE, _.pick(d, 'statedata', 'channelid'));
        break;
      case 'ringing':
        amplify.publish(this.events.RECEIVEDINCOMINGSTATE, _.pick(d, 'statedata', 'channelid'));
        break;
      case 'oncall':
        amplify.publish(this.events.RECEIVEDINSESSIONSTATE, _.pick(d, 'statedata', 'channelid'));
        break;
      case 'wrapup':
        amplify.publish(this.events.RECEIVEDWRAPUPSTATE, _.pick(d, 'statedata', 'channelid'));
        break;
      default:
        break;
      }
    });

    this.addMessageHandler(function (d) {
      if (d.command === 'endchannel') return true;

      return false;
    }, function (d) {
      this._set('isProcessing', false);
      this._set('isCcOnHold', false);

      amplify.publish(this.events.RECEIVEDENDCHANNEL, _.pick(d, 'timestamp', 'channelid'));
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'hold_update') return true;

      return false;
    }, function (d) {
      amplify.publish(this.events.UPDATEDHOLD, _.pick(d, 'status', 'channelid'));
    });

    this.addMessageHandler(function (d) {
      if (d.command === 'arelease') return true;

      return false;
    }, function (d) {
      this._set('realeaseStateChanging', false);

      if (d['releaseData']) {
        this._set('isReleased', true);
        amplify.publish(this.events.RECEIVEDRELEASEDSTATE, _.pick(d, 'releaseData', 'changeTime'));
      }
      else {
        this._set('isReleased', false);
        amplify.publish(this.events.RECEIVEDAVAILABLESTATE);
        amplify.publish(this.events.RECEIVEDIDLESTATE);
      }
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'my_rolling_stats_update') return true;

      return false;
    }, function (d) {
      amplify.publish(this.events.UPDATEDMYROLLINGSTATS, _.pick(d, 'data'));
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'forced_release') return true;

      return false;
    }, function (d) {
      amplify.publish(this.events.FORCEDRELEASE, _.pick(d, 'data'));
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'channel_playback_update') return true;

      return false;
    }, function (d) {
      amplify.publish(this.events.UPDATEDCHANNELPLAYBACK, d.data);
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'outbound_call_update') return true;

      return false;
    }, function (d) {
      if (d.data.state === 'ended') {
        this._set('callingOutbound', false);
        this._set('isOutboundOnHold', false);
      }
      else {
        this._set('callingOutbound', true);
      }

      amplify.publish(this.events.UPDATEDOUTBOUNDCALL, d.data);
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'transfer_agents_update') return true;

      return false;
    }, function (d) {
      amplify.publish(this.events.UPDATEDTRANSFERAGENTS, d.data);
    });

    this.addMessageHandler(function (d) {
      if (d.event === 'conference_update') return true;

      return false;
    }, function (d) {
      amplify.publish(this.events.UPDATEDCONFERENCE, d.data);
    });
  };

  OpenACD.prototype._set = function (propName, value) {
    this[propName] = value;
  };

  OpenACD.prototype._get = function (propName) {
    return this[propName];
  };

  // status things
  OpenACD.prototype.isConnected = false;
  OpenACD.prototype.realeaseStateChanging = false;
  OpenACD.prototype.isReleased = true;
  OpenACD.prototype.isProcessing = false;
  OpenACD.prototype.isCcOnHold = false;
  OpenACD.prototype.callingOutbound = false;
  OpenACD.prototype.isOutboundOnHold = false;

  OpenACD.prototype.setBaseUrls = function () {
    this.getWsockUrls(function (wsUrls) {
      this.baseUrls = _.map(wsUrls, function (wsUrl) {
        // status
        // 1: connected, reconnect to same node
        // 0: not connected, attempt reconnect
        // -1: reconnect failed
        var status = (this.baseUrl === wsUrl) ? 1 : 0;
        return {
          url: wsUrl,
          status: status
        };
      }, this);
    }, this);
  };

  OpenACD.prototype.setBaseUrl = function () {
    var obj = _.findWhere(this.baseUrls, {status: 0});
    if (_.isObject(obj)) {
      this.baseUrl = obj.url;
      return true;
    }

    this.baseUrl = this.baseUrls[0].url;
    return false;
  };

  OpenACD.prototype.getBaseUrlStatus = function () {
    var obj = _.findWhere(this.baseUrls, {url: this.baseUrl});
    return obj.status;
  };

  OpenACD.prototype.setBaseUrlStatus = function (status) {
    var obj = _.findWhere(this.baseUrls, {url: this.baseUrl});
    obj.status = status;
  };

  OpenACD.prototype.resetBaseUrlStatus = function () {
    _.each(this.baseUrls, function (obj) {
      obj.status = 0;
    });
  };

  OpenACD.prototype.connect = function () {
    var self = this;
    try {
      this.ws = new WebSocket(this.baseUrl + '?token=' + $.oucUtils.getValueFromSession('token'));
    }
    catch (e) {
      amplify.publish(this.events.WEBSOCKETERROR, e);
    }

    this.ws.onopen = function () {
      // wait for init message
      self.startPing();
    };

    this.ws.onclose = function () {
      self.ondisconnect();
    };

    this.ws.onmessage = function (msg) {
      var d;
      try {
        d = JSON.parse(msg.data);
      }
      catch (e) {}

      var reqId = d.id,
          uname = d.username,
          pong = d.result && _.has(d.result, 'pong');

      if (!pong && self.logPattern.test(msg.data)) console.log('RCV: ' + msg.data);

      if (pong) {
        try {
          var skewUpdate = new Date(d.result.pong).getTime() - new Date().getTime();
          var oldSkewAve = self.serverTimeSkewMs;

          if (self.pongCount) {
            self.pongCount++;
            self.serverTimeSkewMs = Math.round((oldSkewAve * ((self.pongCount - 1) / self.pongCount)) + (skewUpdate / self.pongCount));
          }
          else {
            self.pongCount = 1;
            self.serverTimeSkewMs = Math.round((skewUpdate + oldSkewAve) / 2);
          }

          clearTimeout(self.pong);
        } catch (e) {}
      }
      else if (_.has(d, 'username')) {
        if (uname) {
          self.serverTimeSkewMs = new Date(d.server_time).getTime() - new Date().getTime();
          self.onconnect(d);
        }
        else {
          if (_.has(d, 'login_error') && d.login_error === 'duplicate') {
            window.location = d.data.redirect_location;
          }
          else {
            self.disconnect();
            throw 'No username';
          }
        }
      }
      else if (reqId) {
        var callback = self.callbacks[reqId];
        if (_.isObject(callback)) {
          callback.method.call(callback.context, d);
        }
        delete self.callbacks[reqId];
      }
      else {
        _.each(self.messageHandlers, function (mh) {
          if (mh.condition(d)) {
            mh.callback.call(self, d);
          }
        });
      }
    };

    this.ws.onerror = function (error) {
      amplify.publish(self.events.WEBSOCKETERROR, error);
    };
  };


  OpenACD.prototype.sleep = function () {
    this.stopPing();
    this.callApi('sleep', [this.sleepDuration]);
  };

  OpenACD.prototype.disconnect = function () {
    this.ws.close();
  };

  OpenACD.prototype.onconnect = function (d) {
    var self = this;
    console.log("level is:"+d['security_level']);
    var level = (d['security_level']) ? d['security_level'] : 'agent';
    level = 'supervisor';
    var reflectUpdates = function () {
      this._set('isConnected', true);
      this._set('username', d['username']);
      this._set('level', level);

      amplify.publish(this.events.CONNECTED, d);
    };

    // admin level work-around
    if (level === 'admin') level = 'supervisor';

    //if (level === 'supervisor' && !this.isSupervisorInit) {
      require(['models/openacd/supervisor'], function (oaSupervisor) {
        // extend self to include supervisor functions
        _.extend(self, _.omit(oaSupervisor, 'initialize'));

        // initialize supervisor
        if (_.isFunction(oaSupervisor.initialize)) {
          this.isSupervisorInit = true;
          oaSupervisor.initialize.call(self);
        }
        reflectUpdates.call(self); // notify only when supervisor functions have been loaded
      });
    /*}
    else {
      reflectUpdates.call(self); // notify immediately
    }*/
  };

  OpenACD.prototype.ondisconnect = function () {
    this.stopPing();
    if (this.isConnectionReplaced) {
      this.isConnectionReplaced = false;
      amplify.publish(this.events.DISCONNECTED, {
        'replaced': true
      });
    }
    else if (this.isConnectionKicked) {
      this.isConnectionKicked = false;
      amplify.publish(this.events.DISCONNECTED, {
        'kicked': true
      });
    }
    else {
      amplify.publish(this.events.DISCONNECTED, {});
    }
    this._set('isConnected', false);
  };

  // for server pushed data
  OpenACD.prototype.addMessageHandler = function (condition, callback) {
    if (!_.isFunction(callback)) return;
    if (!_.isFunction(condition)) return;

    this.messageHandlers.push({
      condition: condition,
      callback: callback
    });
  };

  // keep alive
  OpenACD.prototype.startPing = function () {
    var self = this;

    this.ping = setInterval(function () {
      self.callApi('ping');

      self.pong = setTimeout(function () {
        if (self.ws.readyState < 2) {
          self.disconnect();
        }
        else if (self.ws.readyState === 3) {
          // ws is closed but this still fired
          // means stopPing was not called
          self.ondisconnect();
        }
      }, self.pongTimeout);
    }, self.pingInterval);
  };

  OpenACD.prototype.stopPing = function () {
    try {
      clearInterval(this.ping);
      clearTimeout(this.pong);
    } catch (e) {}
  };

  OpenACD.prototype.callApi = function (funName, args, callback, errCallback, ctx) {
    if (this.ws && this.ws.readyState !== 1) return;
    if (!funName) return;
    if (!this._get('isConnected') && funName !== 'ping') return;
    if (!args) args = [];

    this.requestId += 1;
    try {
      this.ws.send(JSON.stringify({
        id: this.requestId,
        method: funName,
        params: args,
        jsonrpc: '2.0'
      }));
    }
    catch (e) {}

    if (_.isFunction(callback)) {
      this.callbacks[this.requestId] = {
        method: callback,
        context: ctx
      };
    }

    if (_.isFunction(errCallback)) {
      this.errCallbacks[this.requestId] = {
        method: errCallback,
        context: ctx
      };
    }
  };


  // fetches info from api
  // saves the info into model via _.set as passed propName
  // accepts and executes callback
  // args: property value
  OpenACD.prototype.getProperty = function (propName, funName, parser, callback, isForced, ctx) {
    if (!propName) return;
    if (!funName) return;

    var toGetNewVal = true;
    var oldVal = this._get(propName);
    if (oldVal) {
      toGetNewVal = false;
    }
    if (_.isArray(oldVal) && oldVal.length < 1) {
      toGetNewVal = true;
    }

    if (toGetNewVal || isForced) {
      this.callApi(funName, [], function (msg) {
        if (msg.result) {
          var val = parser(msg.result);
          this._set(propName, val);

          if (callback || _.isFunction(callback)) {
            callback.call(ctx, val);
          }
        }
      }, function () {
        // retry?
      }, this);
    }
    else {
      if (callback || _.isFunction(callback)) {
        callback.call(ctx, oldVal);
      }
    }
  };

  // get property
  OpenACD.prototype.getConnectionInfo = function (callback, context, isForced) {
    this.getProperty('connectionInfo', 'get_connection_info', function (result) {
      return result;
    }, callback, isForced, context);
  };

  OpenACD.prototype.getContactInfo = function (callback, context, isForced) {
    this.getProperty('contactInfo', 'ouc.get_contact_info', function (result) {
      return result;
    }, callback, isForced, context);
  };

  OpenACD.prototype.getReleaseOptions = function (callback, context, isForced) {
    this.getProperty('releaseOptions', 'get_release_codes', function (result) {
      return result['codes'];
    }, callback, isForced, context);
  };

  OpenACD.prototype.getSkills = function (callback, context, isForced) {
    this.getProperty('skills', 'get_all_skills', function (result) {
      return result['skills'];
    }, callback, isForced, context);
  };

  OpenACD.prototype.getNodes = function (callback, context, isForced) {
    this.getProperty('nodes', 'get_nodes', function (result) {
      return result['reach_nodes'];
    }, callback, isForced, context);
  };

  OpenACD.prototype.getQueues = function (callback, context, isForced) {
    this.getProperty('queues', 'get_queues', function (result) {
      return result['queues'];
    }, callback, isForced, context);
  };

  OpenACD.prototype.getLines = function (callback, context, isForced) {
    this.getProperty('lines', 'ouc.get_lines', function (result) {
      return result['lines'];
    }, callback, isForced, context);
  };

  OpenACD.prototype.getClients = function (callback, context, isForced) {
    this.getProperty('clients', 'ouc.get_clients', function (result) {
      return result;
    }, callback, isForced, context);
  };

  OpenACD.prototype.getPermissionProfile = function (callback, context, isForced) {
      this.getProperty('permissions', 'ouc.get_permission_profile', function (result) {
        return result['permissions'];
      }, callback, isForced, context);
    };

  // call api
  OpenACD.prototype.saveTabLayout = function (tab, layout, gadgets) {
    this.callApi('ouc.set_tab_layout', [tab, layout, gadgets]);
  };

  OpenACD.prototype.doRingTest = function () {
    this.callApi('ring_test');
  };

  OpenACD.prototype.setAvailable = function () {
    if (this._get('realeaseStateChanging') || !this._get('isReleased')) return;

    this._set('realeaseStateChanging', true);
    this.callApi('go_available', []);
  };

  OpenACD.prototype.setReleased = function (reasonId) {
    if (this._get('realeaseStateChanging') || this._get('isReleased')) return;

    this._set('realeaseStateChanging', true);

    if (!reasonId) {
      this.callApi('go_released');
    }
    else {
      this.callApi('go_released', [reasonId]);
    }
  };

  OpenACD.prototype.setContactInfo = function (contactInfo, callback, context) {
    this.callApi('ouc.set_contact_info', [contactInfo], callback, null, context);
  };


  OpenACD.prototype.initiateOutbound = function (client) {
    if (this._get('callingOutbound')) return;

    this.callApi('ouc.initiate_outbound', [{'client': client}]);
  };

  OpenACD.prototype.callOutbound = function (number) {
    if (!this._get('callingOutbound')) return;

    this.callApi('ouc.call_outbound', [number]);
  };

  OpenACD.prototype.holdOutbound = function () {
    if (!this._get('callingOutbound')) return;

    this.callApi('ouc.hold_outbound', [], function (resp) {
      if (resp.result === 'ok') this._set('isOutboundOnHold', true);
    }, null, this);
  };

  OpenACD.prototype.unholdOutbound = function () {
    if (!this._get('callingOutbound')) return;

    this.callApi('ouc.unhold_outbound', [], function (resp) {
      if (resp.result === 'ok') this._set('isOutboundOnHold', false);
    }, null, this);
  };

  OpenACD.prototype.hangupOutbound = function () {
    if (!this._get('callingOutbound')) return;

    this.callApi('ouc.hangup_outbound');
  };

  OpenACD.prototype.transferOutbound = function (destination) {
    if (!this._get('callingOutbound')) return;

    this.callApi('ouc.transfer_outbound', [destination]);
  };

  OpenACD.prototype.conferenceOutbound = function (destination) {
    if (!this._get('callingOutbound')) return;

    this.callApi('ouc.conference_outbound', [destination]);
  };

  OpenACD.prototype.callVoicemailOutbound = function (channel, number) {
    if (this._get('callingOutbound')) return;

    this.callApi('ouc.call_voicemail_outbound', [channel, number]);
  };

  OpenACD.prototype.hangup = function (channel, callback, context) {
    this.callApi('hangup', [channel], callback, null, context);
  };

  OpenACD.prototype.pause = function (channel, callback, context) {
    this.callApi('pause', [channel], callback, null, context);
  };

  OpenACD.prototype.play = function (channel, positionMs, callback, context) {
    if (!_.isFinite(positionMs)) positionMs = 0;
    this.callApi('play', [channel, {
      location: positionMs
    }], callback, null, context);
  };

  OpenACD.prototype.hold = function (channel, callback, context) {
    this.callApi('hold_channel', [channel], function (resp) {
      if (resp.result === 'success') {
        if (this._get('callingOutbound')) this._set('isOutboundOnHold', true);

        if (_.isFunction(callback)) callback.call(context, resp);
      }
    }, null, this);
  };

  OpenACD.prototype.unhold = function (channel, callback, context) {
    this.callApi('unhold_channel', [channel], function (resp) {
      if (resp.result === 'success') {
        if (this._get('callingOutbound')) this._set('isOutboundOnHold', false);

        if (_.isFunction(callback)) callback.call(context, resp);
      }
    }, null, this);
  };

  OpenACD.prototype.transferToQueue = function (channel, queue, options, callback, context) {
    if (!channel || !queue) return;
    if (!options) options = {};
    this.callApi('transfer_to_queue', [channel, queue, options], callback, null, context);
  };

  OpenACD.prototype.transferOutband = function (channel, number, callback, context) {
    this.callApi('transfer_outband', [channel, number], callback, null, context);
  };

  OpenACD.prototype.subscribeTransferAgents = function (callback, context) {
    this.callApi('ouc.subscribe_transfer_agents', [], function (o) {
      if ($.isFunction(callback)) {
        callback.call(context, o.result.transfer_agents);
      }
    }, null, context);
  };

  OpenACD.prototype.unsubscribeTransferAgents = function (callback, context) {
    this.callApi('ouc.unsubscribe_transfer_agents', [], callback, null, context);
  };

  OpenACD.prototype.transferToAgent = function (channel, agent, callback, context) {
    if (!channel || !agent) return;
    this.callApi('transfer_to_agent', [channel, agent], callback, null, context);
  };

  OpenACD.prototype.conferenceToAgent = function (channel, agent, callback, context) {
    if (!channel || !agent) return;
    this.callApi('conference_to_agent', [channel, agent], callback, null, context);
  };

  OpenACD.prototype.conferenceToQueue = function (channel, queue, options, callback, context) {
    if (!channel || !queue) return;
    if (!options) options = {};
    this.callApi('conference_to_queue', [channel, queue, options], callback, null, context);
  };

  OpenACD.prototype.conferenceOutband = function (channel, number, callback, context) {
    this.callApi('conference_to_outband', [channel, number], callback, null, context);
  };

  OpenACD.prototype.removeFromConference = function (channel, id, callback, context) {
    if (!channel || !id) return;
    this.callApi('remove_from_conference', [channel, id], callback, null, context);
  };

  OpenACD.prototype.endWrapup = function (channel, callback, context) {
    this.callApi('end_wrapup', [channel], callback, null, context);
  };

  OpenACD.prototype.getClient = function (client, callback, context) {
    var getCachedClient = function (clients, clientName) {
      return _.find(clients, function (clientObj) {
        return clientObj['name'] === clientName;
      });
    };

    if (_.isArray(this._get('clients'))) {
      var cc = getCachedClient(this._get('clients'), client);
      if (_.isObject(cc)) {
        if (_.isFunction(callback)) {
          callback.call(context, cc);
        }
      }
      else {
        // fetch missing
        this.callApi('ouc.get_client', [client], function (obj) {
          if (_.isFunction(callback)) {
            callback.call(context, obj.result);
          }
          // check if it's already in the array for whatever reason
          if (_.where(this._get('clients'), {'name': obj.result.name}).length === 0) {
            this.clients.push(obj.result);
          }
        }, null, this);
      }
    }
    else {
      // fetch all
      this.getClients(function (result) {
        if (_.isFunction(callback)) {
          callback.call(context, (getCachedClient(result, client)));
        }
      });
    }
  };

  OpenACD.prototype.getMyRollingStats = function (callback, context) {
    this.callApi('ouc.get_my_rolling_stats', [], function (o) {
      if (_.isFunction(callback)) {
        callback.call(context, o.result);
      }
    }, null, context);
  };

  OpenACD.prototype.getLiveStats = function (callback, context) {
    this.callApi('ouc.get_live_stats', [], function (o) {
      if (_.isFunction(callback)) {
        callback.call(context, o.result.live_stats);
      }
    }, null, context);
  };

  OpenACD.prototype.getMatchedAgentCount = function (queue, skills, callback, context) {
    this.callApi('ouc.get_matched_agent_count', [queue, skills], function (o) {
      if (_.isFunction(callback)) {
        callback.call(context, o.result);
      }
    }, null, context);
  };

  OpenACD.prototype.getWsockUrls = function (callback, context) {
    this.callApi('ouc.get_wsock_urls', [], function (o) {
      if (_.isFunction(callback)) {
        if (window.location.protocol === 'http:') {
          callback.call(context, o.result.ws);
        }
        else {
          callback.call(context, o.result.wss);
        }
      }
    }, null, context);
  };

  return OpenACD;
});
