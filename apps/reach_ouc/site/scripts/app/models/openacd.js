define([
  'knockout',
  'config',
  'models/openacd/core'
], function (ko, config, OpenACD) {
  // simple layer over openacd/core
  // with ko dependency and has observables for properties
  // singleton instance (see _.once)
  // tightly coupled with gadgets
  var OpenACDApp = _.once(function () {
    this._set = function (propName, value) {
      if ($.isFunction(this[propName])) {
        this[propName].call(this, value);
      }
      else {
        if ($.isArray(value)) {
          this[propName] = ko.observableArray(value);
        }
        else {
          this[propName] = ko.observable(value);
        }
      }
    };

    this._get = function (propName) {
      if (!$.isFunction(this[propName])) return null;
      return this[propName].call(this);
    };

    this.reset = function () {
      this.connectionInfo(null);
      this.contactInfo(null);
      this.releaseOptions([]);
      this.skills([]);
      this.nodes([]);
      this.clients([]);
      this.lines([]);
      this.queues([]);
      this.permissionsQueues([]);
      this.permissionsAgents([]);
      this.permissionsMonitor(true);
      this.permissionsBarge(true);
      this.permissionsTransferToQueue(true);
      this.permissionsConfToQueue(true);
      this.permissionsTransferToAgent(true);
      this.permissionsConfToAgent(true);
      this.permissionsTransferToNumber(true);
      this.permissionsConfToNumber(true);
      this.permissionsTransferSkills(true);
      this.permissionsControlAgentState(true);
      this.permissionsWidgets([]);
      this.permissionsCustomizeDesktop(true);
      this.permissionsTabReports(true);
      this.permissionsTabSupervisor(true);

      this.isConnected(false);
      this.realeaseStateChanging(false);
      this.isReleased(true);
      this.isProcessing(false);
      this.isCcOnHold(false);
      this.callingOutbound(false);
      this.isOutboundOnHold(false);

      this.sessionState('released');
      this.channelState('idle');
      this.outboundState('ended');

      this.currentChannel('');
      this.resetCurrentChannel();

      this.outboundSessionStartTs = 0;
      this.outboundCustomer = '';
      this.outboundDestination = '';

      this.conferenceList([]);
      this.isConfOnHold(false);
      this.sessionDurations([]);
    };

    this.resetCurrentChannel = function () {
      this.ccBrandName('');
      this.ccCallerIdName('');
      this.ccCallerIdNumber('');
      this.ccMediaType('');
      this.ccSourceModule('');
      this.ccQueue('');
      this.ccRequestedSkills([]);
      this.ccMatchedSkills([]);
      this.ccClientSkill('');
      this.ccClientLogo('');
      this.ccClientUrlpop('');
      this.ccClientDispVars([]);
      this.ccClientCallDispositions([]);
      this.ccStartTs = 0;
      this.ccSessionStartTs = 0;
      this.ccSessionEndTs = 0;
      this.ccWrapUpTimeLimit = 0;
      this.isCcPaused(false);
      this.isCcStopped(false);
      this.isCcPlaybackUpdated(false);
      this.ccTotalPlaybackMs = 0;
      this.ccCurrentPlaybackMs = 0;
    };

    this.getServerTime = function () {
      return (new Date()).getTime() + this.serverTimeSkewMs;
    };

    this.filterBasicSkills = function (skills) {
      return _.filter(skills, function (sk) {
        if (_.isString(sk)) return sk.indexOf('_') !== 0;
        return false;
      });
    };

    this.filterCsgSkills = function (skills) {
      return _.reduce(skills, function (memo, sk) {
        if (_.isObject(sk)) {
          if (_.isString(sk.client)) {
            memo.push(sk.client);
          }
        }
        return memo;
      }, []);
    };

    this.getStateText = function (observableState, key) {
      if (!key) key = 'sessionState';
      return ko.computed(function () {
        var state = observableState();
        var obj = _.find(this.agentStateList(), function (st) {
          return st[key] === state;
        });

        return obj ? obj.name.toLowerCase() : '';
      }, this);
    };

    this.getLatestStartTs = function (callStateChanges) {
      for (var i = callStateChanges.length - 1; i >= 0; i--) {
        var change = callStateChanges[i];
        if (change.inqueue) {
          var prev = callStateChanges[i - 1];
          if (prev.init) {
            return prev.init;
          }
          else {
            return change.inqueue;
          }
        }
      }
      return 0;
    };

    this.saveSessionDuration = function () {
      if (this.ccSessionStartTs && this.ccSessionEndTs) {
        var duration = (this.ccSessionEndTs - this.ccSessionStartTs);
        this.sessionDurations.push(duration);
        this.ccSessionSaved = true;
      }
    };

    this.isSessionState = function (state) {
      return ko.computed(function () {
        return this.sessionState() === state;
      }, this);
    };
    this.isNotSessionState = function (state) {
      return ko.computed(function () {
        return this.sessionState() !== state;
      }, this);
    };

    this.getContactInfoField = function (key) {
      return ko.computed(function () {
        var info = this.contactInfo();
        if (!_.isObject(info)) return '';

        return info[key] ? info[key] : '';
      }, this);
    };

    this.getContactInfoNestedField = function (parentKey, key) {
      return ko.computed(function () {
        var info = this.contactInfo();
        if (!_.isObject(info)) return '';

        var subInfo = info[parentKey];
        if (!_.isObject(subInfo)) return '';

        return subInfo[key] ? subInfo[key] : '';
      }, this);
    };

    //default values for sound notificaton
    this.soundNotificationCheckValue = ko.observable(true);
    this.soundNotificationInterval = ko.observable(60);
    this.soundNotificationCheckValueOld = ko.observable(true);
    this.soundNotificationIntervalOld = ko.observable(60);
    this.soundNotificationStatus = ko.observable(false);

    this.clearSoundNotification = function() {
       clearInterval(this.soundNotificationStatus);
    }

    this.soundNotificationPlay =function() {
      if(this.soundNotificationCheckValue() == true /*&& this.soundNotificationInterval() > 0*/){
        if (!(window.AudioContext || window.webkitAudioContext)) {
          console.log("Your browser does not support Audio Context.");
          return;
        }
        var duration = this.soundNotificationInterval();
        var ctx = new (window.AudioContext || window.webkitAudioContext);
        var osc = ctx.createOscillator();
        var gainNode = ctx.createGain();
        osc.connect(gainNode);
        osc.type = 'square';
        osc.frequency.value = 3000; // value in hertz
        osc.start(0);
        gainNode.connect(ctx.destination);
        setTimeout(function(){ gainNode.disconnect(ctx.destination); }, 250 );

        this.soundNotificationStatus = setInterval(function() {
          gainNode.connect(ctx.destination);
          setTimeout(function(){
            gainNode.disconnect(ctx.destination);
            }, 250
          );
          }, (duration * 1000)
        );
      }
    };

    // redeclare core fields as observable
    // only manipulated by core
    this.isConnected = ko.observable(false);
    this.realeaseStateChanging = ko.observable(false);
    this.isReleased = ko.observable(true);
    this.isProcessing = ko.observable(false);
    this.isCcOnHold = ko.observable(false);
    this.callingOutbound = ko.observable(false);
    this.isOutboundOnHold = ko.observable(false);

    // dependency of shared info among gadgets
    // to be populated by getProperty calls
    this.connectionInfo = ko.observable();
    this.contactInfo = ko.observable();
    this.releaseOptions = ko.observableArray();
    this.skills = ko.observableArray();
    this.nodes = ko.observableArray();
    this.clients = ko.observableArray();
    this.lines = ko.observableArray();
    this.queues = ko.observableArray();
    this.permissionsQueues = ko.observableArray();
    this.permissionsAgents = ko.observableArray();
    this.permissionsQueueManager = ko.observable();
    this.permissionsAgentManager = ko.observable();
    this.permissionsMonitor = ko.observable();
    this.permissionsBarge = ko.observable();
    this.permissionsTransferToQueue = ko.observable();
    this.permissionsConfToQueue = ko.observable();
    this.permissionsTransferToAgent = ko.observable();
    this.permissionsConfToAgent = ko.observable();
    this.permissionsTransferToNumber = ko.observable();
    this.permissionsConfToNumber = ko.observable();
    this.permissionsTransferSkills = ko.observable();
    this.permissionsControlAgentState = ko.observable();
    this.permissionsWidgets = ko.observableArray();
    this.permissionsCustomizeDesktop = ko.observable();
    this.permissionsTabReports = ko.observable();
    this.permissionsTabSupervisor = ko.observable();

    this.rStatsCoverageList = ko.observableArray([{
      name: 'Last 15 Minutes',
      apiText: 'last_15m'
    }, {
      name: 'Last 30 Minutes',
      apiText: 'last_30m'
    }, {
      name: 'Last Hour',
      apiText: 'last_hr'
    }, {
      name: 'Today',
      apiText: 'today'
    }, {
      name: 'This Week',
      apiText: 'this_week'
    }, {
      name: 'This Month',
      apiText: 'this_month'
    }]);

    this.basicSkills = ko.computed(function () {
      var skills = this.skills();
      var arr = _.flatten(_.reject(skills, function (sks, name) {
        return (name === "Magic");
      }));

      return arr.sort();
    }, this);

    // name - ui display string
    // apiText - text from api
    // sessionState - oa sessionState
    this.agentStateList = ko.observableArray([{
      name: 'Released',
      apiText: 'released',
      sessionState: 'released'
    }, {
      name: 'Idle',
      apiText: 'idle',
      sessionState: 'idle'
    }, {
      name: 'Pre-call',
      apiText: 'prering',
      sessionState: 'precall'
    }, {
      name: 'Ringing',
      apiText: 'ringing',
      sessionState: 'incoming'
    }, {
      name: 'In Session',
      apiText: 'oncall',
      sessionState: 'in_session'
    }, {
      name: 'Wrap-up',
      apiText: 'wrapup',
      sessionState: 'wrap_up'
    }]);

    // status/channel things
    this.sessionState = ko.observable('released');
    this.channelState = ko.observable('idle');
    this.outboundState = ko.observable('ended');

    this.currentChannel = ko.observable('');
    this.ccBrandName = ko.observable('');
    this.ccCallerIdName = ko.observable('');
    this.ccCallerIdNumber = ko.observable('');
    this.ccMediaType = ko.observable('');
    this.ccSourceModule = ko.observable('');
    this.ccQueue = ko.observable('');
    this.ccRequestedSkills = ko.observableArray();
    this.ccMatchedSkills = ko.observableArray();
    this.ccClientSkill = ko.observable();
    this.ccClientLogo = ko.observable('');
    this.ccClientUrlpop = ko.observable('');
    this.ccClientDispVars = ko.observableArray();
    this.ccClientCallDispositions = ko.observableArray();
    this.ccStartTs = 0;
    this.ccSessionStartTs = 0;
    this.ccSessionEndTs = 0;
    this.ccSessionSaved = false;
    this.ccWrapUpTimeLimit = 0;
    this.isCcPaused = ko.observable(false);
    this.isCcStopped = ko.observable(false);
    this.isCcPlaybackUpdated = ko.observable(false);
    this.ccTotalPlaybackMs = 0;
    this.ccCurrentPlaybackMs = 0;

    this.outboundSessionStartTs = 0;
    this.outboundCustomer = '';
    this.outboundDestination = '';

    this.conferenceList = ko.observableArray();
    this.isConfOnHold = ko.observable(false);
    this.sessionDurations = ko.observableArray();

    // contact info related
    this.profilePic = this.getContactInfoField('avatar');
    this.firstName = this.getContactInfoField('first_name');
    this.lastName = this.getContactInfoField('last_name');
    this.position = this.getContactInfoField('position');
    this.manager = this.getContactInfoField('manager');
    this.phoneInternal = this.getContactInfoNestedField('phone', 'internal');
    this.phoneDirect = this.getContactInfoNestedField('phone', 'direct');
    this.phoneCell = this.getContactInfoNestedField('phone', 'cell');
    this.email = this.getContactInfoField('email');
    this.homeAddressStreet = this.getContactInfoNestedField('home_address', 'street');
    this.homeAddressCity = this.getContactInfoNestedField('home_address', 'city');
    this.homeAddressState = this.getContactInfoNestedField('home_address', 'state');
    this.homeAddressCountry = this.getContactInfoNestedField('home_address', 'country');
    this.homeAddressZip = this.getContactInfoNestedField('home_address', 'zip');
    this.fullName = ko.computed(function () {
      var firstName = this.firstName() ? this.firstName() : '';
      var lastName = this.lastName() ? this.lastName() : '';
      return (firstName + ' ' + lastName).trim();
    }, this);
    this.homeAddressText = ko.computed(function () {
      return _.compact([this.homeAddressStreet(), this.homeAddressCity(),
        this.homeAddressState(), this.homeAddressCountry(),
        this.homeAddressZip()]).join(', ');
    }, this);
    this.area = ko.computed(function () {
      return _.compact([this.homeAddressCity(),
        this.homeAddressState()]).join(', ');
    }, this);

    this.myCpt = ko.computed(function () {
      var times = this.sessionDurations();
      if (times.length < 1) return 'infinity';

      var total = _.reduce(times, function (x, acc) {
        return x + acc;
      }, 0);
      return (total / times.length) / 1000;
    }, this);

    this.sessionStateText = this.getStateText(this.sessionState);

    this.sessionStateIconStyle = {
      'released': this.isSessionState('released'),
      'idle': this.isSessionState('idle'),
      'precall': this.isSessionState('precall'),
      'incoming': this.isSessionState('incoming'),
      'outgoing': this.isSessionState('outgoing'),
      'session': this.isSessionState('in_session'),
      'wrap': this.isSessionState('wrap_up')
    };

    this.isReleased.subscribe(function (r) {
      if (r && this.sessionState() === 'idle') {
        this.sessionState('released');
      }
      else if (!r && this.sessionState() === 'released') {
        this.sessionState('idle');
      }
    }, this);

    amplify.subscribe(this.events.RECEIVEDPRECALLSTATE, this, function (obj) {
      this.currentChannel(obj.channelid);
      // TODO verify that other events have the same id
      // TODO verify type is voice and from proper endpoint

      this.ccSessionSaved = false;
      this.channelState('precall');
      this.sessionState('precall');
    });

    amplify.subscribe(this.events.RECEIVEDINCOMINGSTATE, this, function (obj) {
      this.currentChannel(obj.channelid);
      // TODO verify that other events have the same id
      // TODO verify type is voice and from proper endpoint

      var call = obj.statedata; // assume voice
      console.log(call)
      this.ccBrandName(call.brandname);
      this.ccCallerIdName(call.calleridname);
      this.ccCallerIdNumber(call.calleridnumber);
      this.ccMediaType(call.type);
      this.ccSourceModule(call.source_module);
      this.ccQueue(call.queue);
      this.ccStartTs = this.getLatestStartTs(call['state_changes']);
      this.ccRequestedSkills(_.filter(call.initial_skills, function (sk) {
        return _.isString(sk) && sk !== '_all';
      }).sort());
      this.ccMatchedSkills(_.filter(call.skills, function (sk) {
        return _.isString(sk) && sk !== '_all';
      }).sort());
      this.ccClientSkill(this.filterCsgSkills(call.skills)[0]); // assume one

      if (call.source_module === 'freeswitch_voicemail' && call.info) {
        this.ccTotalPlaybackMs = call.info.playback_ms;
      }

      this.getClient(call.brandname, function (client) {
        this.ccClientLogo(client.avatar);
        this.ccClientCallDispositions(client['call_dispositions']);

        if (client.urlpop) {
          var callData = {
            'clientid': encodeURIComponent(call.brandname),
            'callerid': encodeURIComponent(call.calleridname + ' ' + call.calleridnumber),
            'callid': encodeURIComponent(call.callid),
            'type': encodeURIComponent(call.type),
            'media_type': encodeURIComponent(call.media_type),
            'label': null,
            'destination': null,
            'ivroption': null,
            'direction': null
          };

          if (client.urlpop.indexOf('display_variables;') === 0) {
            var varArr = client.urlpop.replace(/^display_variables\;/, '').split(',');
            var variables = _.map(varArr, function (variable) {
              var obj = {};

              if (variable.indexOf('=') !== -1) {
                var pair = variable.split('=');

                obj.key = pair[0];

                if (pair[1].indexOf('oa_') === 0) {
                  var key = pair[1].replace('oa_', '');
                  obj.value = call.url_vars[key];
                }
                else {
                  obj.value = decodeURIComponent(callData[pair[1]]);
                }
              }

              return obj;
            });

            this.ccClientDispVars(variables);
          }
          else {
            _.each(call.url_vars, function (value, key) {
              callData['x_' + key] = value;
            });

            var urlpopEval = client.urlpop.replace(/#\{(.+?)\}/g, function (match) {
              var key = match.substring(2, match.length - 1);
              return callData[key];
            });

            this.ccClientUrlpop(urlpopEval);
          }
        }
      }, this);

      this.channelState('incoming');
      this.sessionState('incoming');
    });

    amplify.subscribe(this.events.RECEIVEDOUTGOINGSTATE, this, function () {
      this.channelState('outgoing');
      this.sessionState('outgoing');
    });

    amplify.subscribe(this.events.RECEIVEDINSESSIONSTATE, this, function (obj) {
      this.ccStartTs = this.getLatestStartTs(obj.statedata['state_changes']);

      this.ccSessionStartTs = _.foldl(obj.statedata['state_changes'], function (memo, change) { if (change.oncall) return change.oncall; else return memo; }, 0);

      this.channelState('in_session');
      this.sessionState('in_session');
    });

    amplify.subscribe(this.events.RECEIVEDWRAPUPSTATE, this, function (obj) {
      this.ccSessionEndTs = _.foldl(obj.statedata['state_changes'], function (memo, change) { if (change.wrapup) return change.wrapup; else return memo; }, 0);

      if (_.isFinite(obj.statedata.time_limit)) {
        if (obj.statedata.time_limit < 0) {
          this.ccWrapUpTimeLimit = 0;
        }
        this.ccWrapUpTimeLimit = obj.statedata.time_limit + this.serverTimeSkewMs;
      }
      else {
        this.ccWrapUpTimeLimit = null;
      }

      this.saveSessionDuration();

      this.conferenceList([]);

      this.channelState('wrap_up');
      this.sessionState('wrap_up');
    });

    amplify.subscribe(this.events.RECEIVEDENDCHANNEL, this, function (obj) {
      if (obj.channelid !== this.currentChannel()) {
        return;
      }

      this.ccSessionEndTs = obj.timestamp;
      this.saveSessionDuration();

      this.resetCurrentChannel();
      this.conferenceList([]);

      if (!this.callingOutbound() && this.isReleased()) {
        this.sessionState('released');
      }
      else {
        var ostate = this.outboundState();
        switch (ostate) {
        case 'initiated':
        case 'awaiting_destination':
          this.sessionState('precall');
          break;
        case 'outgoing_ringing':
          this.sessionState('outgoing');
          break;
        case 'oncall':
          this.sessionState('in_session');
          break;
        default:
          this.sessionState('idle');
          break;
        }
      }

      this.channelState('idle');
      this.currentChannel(null);
    });

    amplify.subscribe(this.events.UPDATEDHOLD, this, function (obj) {
      if (obj.channelid !== this.currentChannel()) {
        return;
      }

      if (obj.status === 'onhold') {
        this.isCcOnHold(true);
      }
      else if (obj.status === 'offhold') {
        if (!this.isConfOnHold()) this.isCcOnHold(false);
      }
      else if (obj.status === 'conference_onhold') {
        this.isConfOnHold(true);
        this.isCcOnHold(true);
      }
      else if (obj.status === 'conference_offhold') {
        this.isConfOnHold(false);
        this.isCcOnHold(false);
      }
    });

    amplify.subscribe(this.events.UPDATEDCHANNELPLAYBACK, this, function (obj) {
      if (obj.source_module !== 'freeswitch_voicemail') return;
      if (obj.channelid !== this.currentChannel()) {
        return;
      }

      switch (obj.type) {
      case 'started':
        this.ccCurrentPlaybackMs = obj.location;
        this.isCcPaused(false);
        this.isCcStopped(false);
        break;
      case 'paused':
        this.isCcPaused(true);
        this.isCcStopped(false);
        break;
      case 'stopped':
        this.ccCurrentPlaybackMs = 0;
        this.isCcPaused(false);
        this.isCcStopped(true);
        break;
      default:
        break;
      }

      this.isCcPlaybackUpdated(true);
    });

    amplify.subscribe(this.events.UPDATEDOUTBOUNDCALL, this, function (obj) {
      // check previous state
      if (this.outboundState() === 'outgoing_ringing') {
        if (obj.reason && obj.reason !== 'hangup') {
          switch (obj.reason) {
          case 'CALL_REJECTED':
            notify('Your outbound call has been rejected.', 'info');
            break;
          case 'EXCHANGE_ROUTING_ERROR':
          case 'UNALLOCATED_NUMBER':
            notify('Your outbound call was not answered.', 'info');
            break;
          default:
            notify('Cannot connect to outbound call destination: ' + obj.reason, 'warn');
            break;
          }
        }
      }

      switch (obj.state) {
      case 'initiated':
      case 'awaiting_destination':
        this.sessionState('precall');
        break;
      case 'outgoing_ringing':
        this.sessionState('outgoing');
        break;
      case 'oncall':
        this.outboundSessionStartTs = obj.timestamp;
        this.sessionState('in_session');
        break;
      case 'ended':
        if (!this.currentChannel() && this.isReleased()) {
          this.sessionState('released');
        }
        else {
          var cstate = this.channelState();
          this.sessionState(cstate);
        }
        break;
      default:
        break;
      }

      this.outboundState(obj.state);
    });

    amplify.subscribe(this.events.UPDATEDCONFERENCE, this, function (n) {
      // TODO: check n.channelid
      var id = n.leg_id;
      if (!id) return;

      var agent = n.agent;
      if (_.isObject(agent)) {
        if (agent.login === this.username()) return;
      }

      var entry = _.findWhere(this.conferenceList(), {id: id});

      if (_.isObject(entry)) {
        if (n.state === 'ended') {
          this.conferenceList.remove(entry);
        }
        else if (n.state === 'declined') {
          entry.reset(n);
        }
        else {
          if (n.state === 'accepted' || n.state === 'offhold') {
            n.state = 'oncall';
            entry.isOnHold(false);
          }
          else if (n.state === 'onhold') {
            entry.isOnHold(true);
          }
          entry.update(n);
        }
      }
      else {
        if (n.state === 'accepted') n.state = 'oncall';
        this.conferenceList.push(new ConferenceEntry(this, n, id));
      }
    });
  });

  var ConferenceEntry  = function (oa, info, id) {
    if (!id) return;
    this.id = id;
    this.type = info.request_type;
    this.value = info.request_value;

    this.state = ko.observable(info.state || '');
    this.stateText = oa.getStateText(this.state, 'apiText');

    this.isOnHold = ko.observable(info.state === 'onhold');

    this.username = ko.observable();
    this.firstName = ko.observable();
    this.lastName = ko.observable();
    this.name = ko.computed(function () {
      var fname = this.firstName();
      var lname = this.lastName();

      fname = fname ? fname.trim() : '';
      lname = lname ? lname.trim() : '';

      return (fname + ' ' + lname).trim();
    }, this);

    this.displayText = ko.computed(function () {
      var username = this.username();
      if (username) {
        var name = this.name();
        var dispName = name ? name : 'agent: ' + username;

        if (this.state() === 'initiated') {
          return 'Waiting for ' + dispName;
        }

        return $.oucUtils.capitalize(dispName);
      }
      else {
        // assume state =  initiated
        var prefix = '';
        switch (this.type) {
        case 'agent':
          prefix = 'Waiting for agent: ';
          break;
        case 'queue':
          prefix = 'Waiting in queue: ';
          break;
        case 'outband':
          prefix = 'External number: ';
          break;
        default:
          console.log('invalid conference type');
          break;
        }
        return prefix + this.value;
      }
    }, this);

    this.update = function (info) {
      this.state(info.state);

      if (_.isObject(info.agent)) {
        this.username(info.agent.login);
        this.firstName(info.agent.first_name || '');
        this.lastName(info.agent.last_name || '');
      }
    };

    this.reset = function (info) {
      this.state(info.state);
      this.username('');
      this.firstName('');
      this.lastName('');
    };

    this.update(info);
  };

  OpenACDApp.prototype = new OpenACD(config);

  return new OpenACDApp();
});
