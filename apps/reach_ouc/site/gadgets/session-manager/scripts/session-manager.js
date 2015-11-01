define([
  'knockout',
  'models/timer',
  'models/gadget',
  'models/openacd',
  'json!../meta/session-manager.json',
  'text!../templates/session-manager.html'
], function (ko, Timer, Parent, oa, meta, template) {
  var SessionMangerVM = function () {
    this.queues = oa.queues;
    this.currentChannel = oa.currentChannel;

    this.isWrapUp = oa.isSessionState('wrap_up');
    this.isInSession = oa.isSessionState('in_session');
    this.isOutgoing = oa.isSessionState('outgoing');
    this.isReleased = oa.isSessionState('released');
    this.isIdle = oa.isSessionState('idle');

    this.brandName = oa.ccBrandName;
    this.callerId = ko.computed(function () {
      return oa.ccCallerIdName() + ' ' +  oa.ccCallerIdNumber();
    });
    this.queue = oa.ccQueue;
    this.brandLogo = oa.ccClientLogo;
    this.brandUrlpop = oa.ccClientUrlpop;
    this.brandDispVars = oa.ccClientDispVars;
    this.brandCallDispositions = oa.ccClientCallDispositions;
    this.isOnHold = oa.isCcOnHold;
    this.isVmailPaused = oa.isCcPaused;
    this.isVmailStopped = oa.isCcStopped;
    this.sessionStateIconStyle = oa.sessionStateIconStyle;
    this.vmailPlaybackTotalMs = ko.observable(oa.ccTotalPlaybackMs);
    this.vmailPlaybackPositionMs = ko.observable(oa.ccCurrentPlaybackMs);
    this.vmailCallbackDestination = ko.observable(oa.ccCallerIdNumber());

    this.transferOptions = [];
    if(oa.permissionsConfToQueue() || oa.permissionsTransferToQueue())
      this.transferOptions.push({text: 'Queue..', val: 'queue'});
    if(oa.permissionsConfToAgent() || oa.permissionsTransferToAgent())
      this.transferOptions.push({text: 'Agent..', val: 'agent'});
    if(oa.permissionsConfToNumber() || oa.permissionsTransferToNumber())
      this.transferOptions.push({text: 'Number..', val: 'number'});

    this.transferOption = ko.observable(null);
    this.transferQueue = ko.observable(null);
    this.transferQueueSkills = ko.observableArray();
    this.transferQueueAgentsEligible = ko.observable();
    this.transferQueueAgentsIdle = ko.observable();
    this.transferNumberBuffer = ko.observable('');
    this.transferNumber = ko.computed(function () {
      return this.transferNumberBuffer();
    }, this).extend({throttle: 300});
    this.transferAgent = ko.observable('');
    this.transferAgentAlerts = ko.observableArray();
    this.transferAgents = ko.observableArray();
    this.transferAgentFilterBuffer = ko.observable('');
    this.transferAgentFilter = ko.computed(function () {
      return this.transferAgentFilterBuffer();
    }, this).extend({ throttle: 300 });
    this.transferAgentsFiltered = ko.computed(function () {
      var filter = this.transferAgentFilter();

      return _.filter(this.transferAgents(), function (ta) {
        var isMatch = true;

        if (filter) {
          filter = filter.toLowerCase();
          isMatch = isMatch && (ta.name().toLowerCase().indexOf(filter) >= 0);
        }

        return isMatch;
      }, this);
    }, this);
    this.transferAgentsSortedBy = ko.observable('name');
    this.transferAgentsSortIsAscendingMap = {
      'name': ko.observable(true),
      'profile': ko.observable(true),
      'state': ko.observable(true)
    };
    this.sortTransferAgentsBy = function (key, keepOrder) {
      if (!key) {
        key = this.transferAgentsSortedBy();
      }

      var isAscending = this.transferAgentsSortIsAscendingMap[key]();

      if (keepOrder) {
        isAscending = !isAscending; // use previous sort order
      }

      this.transferAgents.sort(function (l, r) {
        var left = l[key].call(l);
        var right = r[key].call(l);
        if (isAscending) {
          return left === right ? 0 : (left < right ? -1 : 1);
        }
        else {
          return right === left ? 0 : (right < left ? -1 : 1);
        }
      });

      this.transferAgentsSortedBy(key);
      this.transferAgentsSortIsAscendingMap[key](!isAscending);
    };

    this.isTransferVisible = ko.computed(function () {
      return !!this.currentChannel() && this.isInSession();
    }, this);
    this.isConfEnabled = ko.observable(false);
    this.isTransEnabled = ko.observable(false);
    this.conferenceList = oa.conferenceList;

    this.clientSkill = ko.computed(function () {
      return oa.ccClientSkill() || this.brandName();
    }, this);
    this.skillGroups = ko.computed(function () {
      var brandText = 'Client: ' + this.clientSkill();
      var arr = _.reduce(oa.skills(), function (arr, sks, name) {
        if (name !== "Magic") {
          var obj = {
            name: name,
            skills: _.sortBy(_.map(sks, function (sk) {
              return {
                name: sk,
                value: sk,
                type: 'basic'
              };
            }), function (obj) {
              return obj.name;
            })
          };
          var i = Math.ceil(obj.skills.length / 2);
          obj.skillsCol1 = _.first(obj.skills, i);
          obj.skillsCol2 = _.rest(obj.skills, i);
          arr.push(obj);
        }

        return arr;
      }, []);

      arr.unshift({
        name: '',
        skillsCol1: [{
          name: brandText,
          value: '_brand',
          type: 'magic'
        }]
      });
      return _.sortBy(arr, function (obj) {
        return obj.name;
      });
    }, this);

    this.requestedSkillsText = ko.computed(function () {
      return oa.ccRequestedSkills().join(', ');
    }, this);

    this.matchedSkillsText = ko.computed(function () {
      return oa.ccMatchedSkills().join(', ');
    }, this);


    this.isActive = ko.computed(function () {
      return !this.isReleased() && !this.isIdle();
    }, this);

    this.isProcessing = ko.computed(function () {
      return this.isInSession() || this.isWrapUp();
    }, this);

    this.isVoicemail = ko.computed(function () {
      var mod = oa.ccSourceModule();
      return (mod === 'freeswitch_voicemail') ? true : false;
    });

    this.isConference = ko.computed(function () {
      var mod = oa.ccSourceModule();
      return (mod === 'freeswitch_conference') ? true : false;
    });

    // default for media type voice
    this.isVoiceCall = ko.computed(function () {
      var mod = oa.ccSourceModule();
      var media = oa.ccMediaType();
      return !this.isVoicemail() && !this.isConference() && (media === 'voice');
    }, this);

    this.isVmailCallback = ko.computed(function () {
      return this.isVoicemail() && oa.callingOutbound();
    }, this);

    this.isVmailInSession = ko.computed(function () {
      return (this.isInSession() || this.isOutgoing()) && this.isVoicemail() && !this.isVmailCallback();
    }, this);

    this.mediaTypeText = ko.computed(function () {
      if (this.isVoiceCall()) return 'Call';
      if (this.isVoicemail()) return 'Voicemail';
      if (this.isConference()) return 'Conference';

      return oa.ccMediaType();
    }, this);

    this.vmailPlaybackPositionMMSS = ko.computed(function () {
      var seconds = this.vmailPlaybackPositionMs() / 1000;
      return Timer.prototype.utils.formatMMSS(Math.round(seconds));
    }, this);

    this.vmailPlaybackTotalMMSS = ko.computed(function () {
      var seconds = this.vmailPlaybackTotalMs() / 1000;
      return Timer.prototype.utils.formatMMSS(Math.round(seconds));
    }, this);

    this.canCallbackVmail = ko.computed(function () {
      return this.isVmailInSession() && !this.isVmailCallback();
    }, this);

    this.showSessionClientCallDispositions = ko.computed(function () {
      return (this.isProcessing()) && !_.isEmpty(this.brandCallDispositions());
    }, this);

    this.showCustomerHistoryDiv = ko.computed(function () {
      return (this.brandUrlpop() !== '') || (this.brandDispVars().length > 0);
    }, this);

    this.isTransferOption = function (option) {
      return ko.computed(function () {
        return this.transferOption() === option;
      }, this);
    };

    this.endWrapup = function () {
      oa.endWrapup(oa.currentChannel());
      this.brandUrlpop('');
      this.timer.stop();
      this.waitTimer.stop();
    };

    this.getTransferQueueSkills = function () {
      var tqs = _.partition(this.transferQueueSkills(), function (s) {
        return s !== '_brand';
      });

      if (tqs[1].length > 0) {
        var client = {'client': this.clientSkill()};
        tqs[0].push(client);
      }

      return tqs[0];
    };

    this.transfer = function () {
      var transferOption = this.transferOption();
      if (transferOption === 'queue') {
        oa.transferToQueue(oa.currentChannel(), this.transferQueue(), {
          'skills': this.getTransferQueueSkills()
        });
      }
      else if (transferOption === 'number') {
        oa.transferOutband(oa.currentChannel(), this.transferNumber());
      }
      else if (transferOption === 'agent') {
        oa.transferToAgent(oa.currentChannel(), this.transferAgent());
      }

      this.transferOption(null);
      this.isTransEnabled(false);
    };

    this.conference = function () {
      var transferOption = this.transferOption();
      if (transferOption === 'queue') {
        oa.conferenceToQueue(oa.currentChannel(), this.transferQueue(), {
          'skills': this.getTransferQueueSkills()
        }, this.conferenceCallback, this);
      }
      else if (transferOption === 'number') {
        oa.conferenceOutband(oa.currentChannel(), this.transferNumber(), this.conferenceCallback, this);
      }
      else if (transferOption === 'agent') {
        oa.conferenceToAgent(oa.currentChannel(), this.transferAgent(), this.conferenceCallback, this);
      }
    };

    this.conferenceCallback = function (resp) {
      if (resp.error) {
        this.transferAgentAlerts.push(this.transferAgent());
        this.transferAgent(null);

        switch (resp.error.code) {
        case 4004:
          notify('Agent is not available for conference.', 'warn');
          break;
        default:
          notify('Error ' + resp.error.code + ': ' + resp.error.message, 'error');
          break;
        }
      }
      else if (resp.result) {
        this.transferOption(null);
        this.isConfEnabled(false);
      }
    };

    this.removeFromConference = function (id) {
      oa.removeFromConference(oa.currentChannel(), id);
    };

    this.getTransferQueueAgentMatch = function () {
      oa.getMatchedAgentCount(this.transferQueue(), this.getTransferQueueSkills(), function (obj) {
        this.transferQueueAgentsEligible(_.isFinite(obj.eligible_agents) ? obj.eligible_agents : '-');
        this.transferQueueAgentsIdle(_.isFinite(obj.idle_agents) ? obj.idle_agents : '-');
      }, this);
    };

    this.resetTransfer = function () {
      this.transferQueue(null);
      this.transferQueueSkills([]);
      this.transferQueueAgentsEligible('-');
      this.transferQueueAgentsIdle('-');
      this.transferNumberBuffer(null);
      this.transferAgent(null);
      this.transferAgentAlerts([]);
      this.transferAgentFilterBuffer(null);
      this.transferAgentsSortedBy('name');
      this.transferAgentsSortIsAscendingMap.name(true);
      this.transferAgentsSortIsAscendingMap.profile(true);
      this.transferAgentsSortIsAscendingMap.state(true);
    };

    this.hangup = function () {
      oa.hangup(oa.currentChannel());
      this.waitTimer.stop();
    };

    this.toggleHold = function () {
      var isOnHold = this.isOnHold();
      if (isOnHold) {
        oa.unhold(oa.currentChannel());
      }
      else {
        oa.hold(oa.currentChannel());
      }
    };

    this.vmailCallback = function () {
      oa.outboundCustomer = this.brandName();
      oa.outboundDestination = this.vmailCallbackDestination();
      oa.callVoicemailOutbound(oa.currentChannel(), oa.outboundDestination);
    };

    this.toggleVmailPause = function () {
      var isPaused = this.isVmailPaused();
      var isStopped = this.isVmailStopped();

      if (isPaused || isStopped) {
        if (isStopped) {
          this.vmailPlaybackPositionMs(oa.ccCurrentPlaybackMs);
        }

        oa.ccCurrentPlaybackMs = this.vmailPlaybackPositionMs();

        // console.log(oa.ccCurrentPlaybackMs)
        oa.play(oa.currentChannel(), oa.ccCurrentPlaybackMs);
      }
      else {
        oa.pause(oa.currentChannel());
      }
    };

    this.startVmailSlider = function () {
      if (this.vmailSliderInterval) return;

      // console.log('start slide')
      var self = this;
      this.vmailSliderInterval = setInterval(function () {
        var currentMs = self.vmailPlaybackPositionMs() + 200;
        var totalMs = self.vmailPlaybackTotalMs();
        if (currentMs > totalMs) {
          currentMs = totalMs;
          self.stopVmailSlider();
        }
        self.vmailPlaybackPositionMs(currentMs);
      }, 200);
    };

    this.stopVmailSlider = function () {
      // console.log('stop slide')
      try {
        clearInterval(this.vmailSliderInterval);
        this.vmailSliderInterval = null;
      } catch (e) {}
    };

    this.onVmailSlide = function (event, ui) {
      var isPaused = this.isVmailPaused();
      var isStopped = this.isVmailStopped();

      oa.ccCurrentPlaybackMs = ui.value;

      if (!isPaused && !isStopped) {
        oa.play(oa.currentChannel(), ui.value);
      }
    };

    this.timer = new Timer();
    this.waitTimer = new Timer();

    this.handleState = function (ss) {
      switch (ss) {
      case 'incoming':
        var waitMs = oa.getServerTime() - new Date(oa.ccStartTs).getTime();
        this.waitTimer.start(waitMs / 1000);
        break;
      case 'in_session':
        var waitMs = new Date(oa.ccSessionStartTs).getTime() - new Date(oa.ccStartTs).getTime();
        var answerMs = oa.getServerTime() - new Date(oa.ccSessionStartTs).getTime();
        this.timer.isCountdown(false);
        this.timer.start(answerMs / 1000);
        this.waitTimer.stop();
        this.waitTimer.totalSeconds(waitMs / 1000);
        this.transferOption(null);

        if (this.isVoicemail()) {
          this.vmailPlaybackTotalMs(oa.ccTotalPlaybackMs);
          if (oa.callingOutbound()) {
            this.vmailCallbackDestination(oa.outboundDestination);
          }
          else {
            this.vmailCallbackDestination(oa.ccCallerIdNumber());
          }
        }
        break;
      case 'wrap_up':
        var waitMs = new Date(oa.ccSessionStartTs).getTime() - new Date(oa.ccStartTs).getTime();
        var hangupMs = oa.getServerTime() - new Date(oa.ccSessionEndTs).getTime();
        this.timer.reset();
        if (!_.isNull(oa.ccWrapUpTimeLimit)) {
          var countdownSeconds = oa.ccWrapUpTimeLimit / 1000;
          this.timer.isCountdown(true);
          this.timer.countdownSeconds(countdownSeconds);
        }
        else {
          this.timer.isCountdown(false);
        }
        this.timer.start(hangupMs / 1000);
        this.waitTimer.stop();
        this.waitTimer.totalSeconds(waitMs / 1000);

        if (this.isVoicemail()) {
          this.stopVmailSlider();
        }
        break;
      }
    };

    this.initialize = function () {
      this.handleState(oa.channelState());
      oa.channelState.subscribe(function (ss) { this.handleState(ss); }, this);

      oa.currentChannel.subscribe(function (cc) {
        if (cc) return;

        this.timer.reset();
        this.waitTimer.reset();
      }, this);

      oa.isCcPlaybackUpdated.subscribe(function (hasUpdate) {
        if (hasUpdate) {
          var isPaused = this.isVmailPaused();
          var isStopped = this.isVmailStopped();

          if (isPaused || isStopped) {
            if (isStopped) {
              this.vmailPlaybackPositionMs(this.vmailPlaybackTotalMs());
            }

            this.stopVmailSlider();
          }
          else {
            this.vmailPlaybackPositionMs(oa.ccCurrentPlaybackMs);
            this.startVmailSlider();
          }

          oa.isCcPlaybackUpdated(false);
        }
      }, this);

      amplify.subscribe(oa.events.UPDATEDTRANSFERAGENTS, this, function (updates) {
        var pushList = [];
        var removeList = [];

        _.each(updates, function (n, u) {
          var entry = _.findWhere(this.transferAgents(), {username: u});

          if (_.isObject(n)) {
            if (_.isObject(entry)) {
              entry.update(n);

              if (n.state === 'idle') {
                this.transferAgentAlerts.remove(u);
              }
            }
            else {
              pushList.push(new TransferAgent(n, u));
            }
          }
          else if (n === 'offline') {
            removeList.push(entry);
          }
        }, this);

        if (removeList.length > 0) {
          this.transferAgents.removeAll(removeList);
        }

        if (pushList.length > 0) {
          this.transferAgents.push.apply(this.transferAgents, pushList);
          this.sortTransferAgentsBy(null, true);
        }
      });

      this.fetchInit();
    };

    this.reset = function () {
      this.vmailPlaybackTotalMs(oa.ccTotalPlaybackMs);
      this.vmailPlaybackPositionMs(oa.ccCurrentPlaybackMs);
      this.vmailCallbackDestination(oa.ccCallerIdNumber());

      this.transferOption(null);
      this.transferQueue(null);
      this.transferQueueSkills([]);
      this.transferQueueAgentsEligible('-');
      this.transferQueueAgentsIdle('-');
      this.transferNumberBuffer('');
      this.transferAgent('');
      this.transferAgentAlerts([]);
      this.transferAgents([]);
      this.transferAgentFilterBuffer('');
      this.transferAgentsSortedBy('name');
      this.transferAgentsSortIsAscendingMap.name(true);
      this.transferAgentsSortIsAscendingMap.profile(true);
      this.transferAgentsSortIsAscendingMap.state(true);
      this.isTransEnabled(false);
      this.isConfEnabled(false);

      this.fetchInit();
    };

    this.fetchInit = function () {
      oa.getQueues();
      oa.getSkills();
    };

    this.transferOption.subscribe(function (newVal, oldVal) {
      this.resetTransfer();

      if (newVal === 'agent') {
        oa.subscribeTransferAgents(function (result) {
          var transferAgents = _.map(result, function (x, u) {
            return new TransferAgent(x, u);
          }, this);

          this.transferAgents(transferAgents);
          this.sortTransferAgentsBy(null);
        }, this);
      }
      else {
        oa.unsubscribeTransferAgents();
      }
    }, this);

    this.transferQueue.subscribe(function (val) {
      if (val) {
        var queueObj = _.findWhere(this.queues(), {name: val});
        var queueSk = [];
        if (queueObj) {
          queueSk = queueObj.skills || [];
          this.transferQueueSkills(_.filter(queueSk, function (sk) {
            if (_.isString(sk)) {
              if (sk.indexOf('_') !== 0) return true;
              else if (sk === '_brand') return true;
              else return false;
            }
            else return false;
          }));
        }
        if(oa.permissionsTransferToQueue())
          this.isTransEnabled(true);
        if(oa.permissionsConfToQueue())
          this.isConfEnabled(true);

        if (queueSk.length === 0) this.getTransferQueueAgentMatch();
      }
      else {
        this.isTransEnabled(false);
        this.isConfEnabled(false);
      }
    }, this);

    this.transferNumber.subscribe(function (val) {
      if (val) {
        if(oa.permissionsTransferToNumber())
          this.isTransEnabled(true);
        if(oa.permissionsConfToNumber())
          this.isConfEnabled(true);
      }
      else {
        this.isTransEnabled(false);
        this.isConfEnabled(false);
      }
    }, this);

    this.transferAgentFilterBuffer.subscribe(function () {
      this.transferAgent(null);
    }, this);

    this.transferAgent.subscribe(function (val) {
      if (val) {
        if(oa.permissionsTransferToAgent())
          this.isTransEnabled(true);
        if(oa.permissionsConfToAgent())
          this.isConfEnabled(true);
      }
      else {
        this.isTransEnabled(false);
        this.isConfEnabled(false);
      }
    }, this);

    this.transferQueueSkills.subscribe(function () {
      if (this.transferQueue()) {
        this.getTransferQueueAgentMatch();
      }
    }, this);
  };

  var TransferAgent = function (info, username) {
    if (!username) return;

    this.username = username;
    this.firstName = ko.observable(info.first_name || '');
    this.lastName = ko.observable(info.last_name || '');
    this.name = ko.computed(function () {
      var fname = this.firstName();
      var lname = this.lastName();

      fname = fname ? fname.trim() : '';
      lname = lname ? lname.trim() : '';

      return (fname + ' ' + lname).trim();
    }, this);
    this.profile = ko.observable(info.profile || '');
    this.state = ko.observable(info.state || '');
    this.stateText = oa.getStateText(this.state, 'apiText');

    this.update = function (info) {
      this.firstName(info.first_name);
      this.lastName(info.last_name);
      this.profile(info.profile);
      this.state(info.state);
    };
  };

  var sortByBinding = function (key, vm) {
    return {
      attr: {
        title: vm.transferAgentsSortIsAscendingMap[key]() ? 'sort ascending' : 'sort descending'
      },
      css: {
        'active': vm.transferAgentsSortedBy() === key
      },
      event: {
        click: function () {
          vm.sortTransferAgentsBy(key);
        }
      }
    };
  };

  var bindings = {
    smSessionStateIcon: function () {
      return {
        css: this.sessionStateIconStyle
      };
    },
    smVisibleWhenValidChannel: function () {
      return {
        visible: !!this.currentChannel()
      };
    },
    smVisibleWhenActive: function () {
      return {
        visible: this.isActive
      };
    },
    smSessionMediaText: function () {
      return {
        text: this.mediaTypeText
      };
    },
    smSessionMediaIcon: function () {
      return {
        css: {
          'session-call': this.isVoiceCall,
          'media-voice': this.isVoicemail,
          'media-conf': this.isConference
        }
      };
    },
    smSessionRequestedSkillsText: function () {
      return {
        text: this.requestedSkillsText
      };
    },
    smSessionMatchedSkillsText: function () {
      return {
        text: this.matchedSkillsText
      };
    },
    smSessionQueueText: function () {
      return {
        text: this.queue
      };
    },
    smSessionWaitTimeText: function () {
      return {
        text: this.waitTimer.toMMSS
      };
    },
    smSessionClientText: function () {
      return {
        text: this.brandName
      };
    },
    smSessionClientCallerId: function () {
      return {
        text: this.callerId
      };
    },
    smVisibleWhenProcessing: function () {
      return {
        visible: this.isProcessing
      };
    },
    smSessionTimer: function () {
      return {
        css: {
          'warn': this.timer.isCountdownOver
        }
      };
    },
    smSessionTimerText: function () {
      return {
        text: this.timer.toMMSS
      };
    },
    smVisibleWhenNotWrapup: function () {
      return {
        visible: !this.isWrapUp()
      };
    },
    smVisibleWhenWrapup: function () {
      return {
        visible: this.isWrapUp
      };
    },
    smEndWrapupButton: function () {
      return {
        click: this.endWrapup
      };
    },
    smHangupButton: function () {
      return {
        click: this.hangup
      };
    },
    smHoldButton: function () {
      return {
        visible: this.isVoiceCall() || this.isConference() || this.isVmailCallback(),
        click: this.toggleHold,
        css: {
          'active': this.isOnHold
        }
      };
    },
    smConferenceDiv: function () {
      return {
        visible: this.conferenceList().length > 0
      };
    },
    smConfList: function () {
      return {
        foreach: this.conferenceList
      };
    },
    smConfEntryText: function () {
      return {
        text: this.displayText
      };
    },
    smConfEntryStatusIcon: function () {
      return {
        css: {
          'released': this.state() === 'released',
          'idle': this.state() === 'idle',
          'precall': this.state() === 'prering',
          'incoming': this.state() === 'ringing',
          'session': this.state() === 'oncall',
          'wrap': this.state() === 'wrapup'
        }
      };
    },
    smConfEntryStatusText: function () {
      return {
        text: this.isOnHold() ? 'on hold' : this.stateText()
      };
    },
    smConfEntryButton: function (ctx) {
      var id = this.id;
      return {
        click: function () {
          ctx.$root.removeFromConference(id);
        }
      };
    },
    smVoicemailDiv: function () {
      return {
        visible: this.isVmailInSession
      };
    },
    smVoicemailSlider: function (ctx) {
      return {
        slider: this.vmailPlaybackPositionMs,
        sliderOptions: {
          orientation: 'horizontal',
          min: 0,
          max: this.vmailPlaybackTotalMs,
          step: 1,
          stop: this.onVmailSlide.bind(ctx.$data)
        }
      };
    },
    smVoicemailPosition: function () {
      return {
        text: this.vmailPlaybackPositionMMSS
      };
    },
    smVoicemailTotal: function () {
      return {
        text: this.vmailPlaybackTotalMMSS
      };
    },
    smVoicemailPlaybackButton: function () {
      return {
        click: this.toggleVmailPause
      };
    },
    smVoicemailPlaybackButtonIcon: function () {
      return {
        css: {
          'icon-pause': !this.isVmailPaused(),
          'icon-play': this.isVmailPaused() || this.isVmailStopped()
        }
      };
    },
    smVoicemailCallbackInput: function () {
      return {
        enable: this.canCallbackVmail,
        value: this.vmailCallbackDestination
      };
    },
    smVoicemailCallbackButton: function () {
      return {
        css: {
          'active': this.isOutgoing
        },
        enable: this.canCallbackVmail,
        click: this.vmailCallback
      };
    },
    smSessionLogo: function () {
      return {
        attr: {
          src: this.brandLogo
        },
        event: {
          load: function (vm, e) {
            $(e.currentTarget).show();
            $(e.currentTarget).parent().css('background', 'none');
          },
          error: function (vm, e) {
            $(e.currentTarget).hide();
            $(e.currentTarget).parent().removeAttr('style');
          }
        }
      };
    },
    smTransferDiv: function () {
      return {
        visible: this.isTransferVisible
      };
    },
    smTransferForm: function () {
      return {
        css: {
          'queue': this.isTransferOption('queue'),
          'agent': this.isTransferOption('agent'),
          'number': this.isTransferOption('number')
        }
      };
    },
    smTransferSelect: function () {
      return {
        options: this.transferOptions,
        optionsText: 'text',
        optionsValue: 'val',
        value: this.transferOption,
        optionsCaption: 'Transfer / Conference'
      };
    },
    smTransferButton: function () {
      return {
        enable: this.isTransEnabled,
        click: this.transfer
      };
    },
    smConferenceButton: function () {
      return {
        enable: this.isConfEnabled,
        click: this.conference
      };
    },
    smVisibleWhenTransferEnabled: function () {
      return {
        visible: this.isTransEnabled() || this.isConfEnabled()
      };
    },
    smVisibleWhenQueueTransfer: function () {
      return {
        visible: this.isTransferOption('queue')
      };
    },
    smVisibleWhenNumberTransfer: function () {
      return {
        visible: this.isTransferOption('number')
      };
    },
    smVisibleWhenAgentTransfer: function () {
      return {
        visible: this.isTransferOption('agent')
      };
    },
    smQTransSelect: function () {
      return {
        options: this.queues,
        optionsText: 'name',
        optionsValue: 'name',
        value: this.transferQueue,
        optionsCaption: 'Select Queue...'
      };
    },
    smVisibleWhenQTransSelected: function () {
      return {
        visible: this.transferQueue() && oa.permissionsTransferSkills()
      };
    },
    smQTransSkillGroups: function () {
      return {
        foreach: this.skillGroups
      };
    },
    smQTransSkillGroupText: function () {
      return {
        text: this.name
      };
    },
    smQTransSkillsCol1: function () {
      return {
        foreach: this.skillsCol1
      };
    },
    smQTransSkillsCol2: function () {
      return {
        foreach: this.skillsCol2
      };
    },
    smQTransSkillText: function () {
      return {
        text: this.name,
        css: {
          'magic-skill': this.type === 'magic'
        }
      };
    },
    smQTransSkillCheckbox: function (ctx) {
      return {
        checked: ctx.$root.transferQueueSkills,
        value: this.value
      };
    },
    smQTransAgEligible: function () {
      return {
        text: this.transferQueueAgentsEligible
      };
    },
    smQTransAgIdle: function () {
      return {
        text: this.transferQueueAgentsIdle
      };
    },
    smNTransInput: function () {
      return {
        value: this.transferNumberBuffer,
        valueUpdate: 'keyup'
      };
    },
    smATransSortByName: function (ctx) {
      return sortByBinding('name', ctx.$root);
    },
    smATransSortByProfile: function (ctx) {
      return sortByBinding('profile', ctx.$root);
    },
    smATransSortByState: function (ctx) {
      return sortByBinding('state', ctx.$root);
    },
    smATransAgentList: function () {
      return {
        foreach: this.transferAgentsFiltered
      };
    },
    smATransFilter: function () {
      return {
        value: this.transferAgentFilterBuffer,
        valueUpdate: 'keyup'
      };
    },
    smATransAgent: function (ctx) {
      return {
        click: function () {
          if (_.contains(ctx.$root.transferAgentAlerts(), this.username)) return;
          ctx.$root.transferAgent(this.username);
        },
        css: {
          'selected': ctx.$root.transferAgent() === this.username,
          'alert': _.contains(ctx.$root.transferAgentAlerts(), this.username)
        }
      };
    },
    smATransAgentName: function () {
      return {
        text: this.name
      };
    },
    smATransAgentProfile: function () {
      return {
        text: this.profile
      };
    },
    smATransAgentStatusIcon: function () {
      return {
        css: {
          'released': this.state() === 'released',
          'idle': this.state() === 'idle',
          'precall': this.state() === 'prering',
          'incoming': this.state() === 'ringing',
          'session': this.state() === 'oncall',
          'wrap': this.state() === 'wrapup'
        }
      };
    },
    smATransAgentStatusText: function () {
      return {
        text: this.stateText
      };
    },
    smCustomerHistoryDiv: function () {
      return {
        visible: this.showCustomerHistoryDiv
      };
    },
    smCustomerHistoryFrame : function () {
      return {
        visible: this.brandUrlpop() !== '',
        attr: {
          src: this.brandUrlpop
        }
      };
    },
    smCustomerHistoryDisplayVariables: function () {
      return {
        visible: this.brandDispVars().length > 0,
      };
    },
    smCustomerHistoryDisplayVariablesBody: function () {
      return {
        foreach: this.brandDispVars
      };
    },
    smCustomerHistoryDisplayVariableKey: function () {
      return {
        text: this.key
      };
    },
    smCustomerHistoryDisplayVariableValue: function () {
      return {
        linkyText: this.value,
        textNone: true
      };
    },
    smCallDispositions: function () {
      return {
        visible: this.showSessionClientCallDispositions
      };
    },
    smCallDispositionsDropdown: function () {
      return {
        options: this.brandCallDispositions
      };
    }
  };

  ko.bindingProvider.instance.registerBindings(bindings);

  return {
    ViewModel: SessionMangerVM,
    Parent: Parent,
    meta: meta,
    template: _.template(template, {})
  };
});
