define([
  'knockout',
  'models/timer',
  'models/gadget',
  'models/openacd',
  'json!../meta/agent-manager.json',
  'text!../templates/agent-manager.html'
], function (ko, Timer, Parent, oa, meta, template) {
  var AgentManagerVM = function () {
    // available covarage in api
    this.availableCoverage = oa.rStatsCoverageList;

    // map to strings displayed in this gadget
    this.states = oa.agentStateList;

    this.customerList = ko.computed(function () {
      return _.map(oa.clients(), function (client) {
        return client.name;
      }).sort();
    });

    this.skillList = oa.basicSkills;

    this.nodeList = ko.computed(function () {
      return oa.nodes().sort();
    });

    this.releaseReasons = ko.computed(function () {
      return _.union([{
        id: 'default',
        name: 'Default'
      }, {
        id: 'available',
        name: 'Available'
      }], ko.toJS(oa.releaseOptions()), [{
        id: 'kick',
        name: 'Log Off'
      }]);
    });

    this.profiles = ko.observableArray();
    this.profileList = ko.computed(function () {
      return _.map(this.profiles(), function (profile) {
        return profile.name;
      }).sort();
    }, this).extend({throttle: 300}); // throttle this to optimize animations
    this.profileStatsCoverage = ko.observable('last_hr');

    this.agents = ko.observableArray();
    this.nameFilterBuffer = ko.observable('');
    this.nameFilter = ko.computed(function () {
      return this.nameFilterBuffer();
    }, this).extend({ throttle: 300 });
    this.profileFilter = ko.observable('');
    this.customerFilter = ko.observable('');
    this.stateFilter = ko.observable('');
    this.skillFilter = ko.observable('');
    this.nodeFilter = ko.observable('');
    this.agentStatsCoverage = ko.observable('last_hr');

    this.filteredAgents = ko.computed(function () {
      var nameFilter = this.nameFilter();
      var profileFilter = this.profileFilter();
      var customerFilter = this.customerFilter();
      var stateFilter = this.stateFilter();
      var skillFilter = this.skillFilter();
      var nodeFilter = this.nodeFilter();

      return _.filter(this.agents(), function (agent) {
        var isMatch = true;

        if (nameFilter) {
          isMatch = isMatch && (agent.name().toLowerCase().indexOf(nameFilter.toLowerCase()) >= 0);
        }

        if (profileFilter) {
          isMatch = isMatch && (agent.profile().toLowerCase() === profileFilter.toLowerCase());
        }

        if (customerFilter) {
          var csg = _.map(agent.csg(), function (c) {
            return c.toLowerCase();
          });
          isMatch = isMatch && (_.contains(csg, customerFilter.toLowerCase()));
        }

        if (stateFilter) {
          isMatch = isMatch && (agent.state().toLowerCase() === stateFilter.toLowerCase());
        }

        if (skillFilter) {
          var skills = _.map(agent.skills(), function (sk) {
            return sk.toLowerCase();
          });
          isMatch = isMatch && (_.contains(skills, skillFilter.toLowerCase()));
        }

        if (nodeFilter) {
          isMatch = isMatch && (agent.node().toLowerCase() === nodeFilter.toLowerCase());
        }

        return isMatch;
      });
    }, this);

    this.agentsSortIsAscendingMap = {
      'name': ko.observable(true),
      'occup': ko.observable(true),
      'calls': ko.observable(true),
      'myCpt': ko.observable(true),
      'loginDuration': ko.observable(true),
      'state': ko.observable(true)
    };
    this.agentsSortedBy = ko.observable('name');
    this.sortByIconTitle = function (key) {
      return ko.computed(function () {
        return this.agentsSortIsAscendingMap[key]() ? 'sort ascending' : 'sort descending';
      }, this);
    };
    this.isSortedBy = function (key) {
      return ko.computed(function () {
        return this.agentsSortedBy() === key;
      }, this);
    };
    this.sortAgentsBy = function (key, keepOrder) {
      if (!key) {
        key = this.agentsSortedBy();
      }

      var isAscending = this.agentsSortIsAscendingMap[key]();

      if (keepOrder) {
        isAscending = !isAscending; // use previous sort order
      }

      this.agents.sort(function (l, r) {
        var left = l[key].call(l);
        var right = r[key].call(l);

        if (key === 'occup') {
          left = parseFloat(left);
          right = parseFloat(right);
        }

        if (isAscending) {
          return left === right ? 0 : (left < right ? -1 : 1);
        }
        else {
          return right === left ? 0 : (right < left ? -1 : 1);
        }
      });

      this.agentsSortedBy(key);
      this.agentsSortIsAscendingMap[key](!isAscending);
    };

    this.Agent = function (x, statCoverage) {
      this.AgentRollingStat = function (rstat) {
        this.coverage = rstat.coverage;
        this.occup = ko.observable(rstat.occupancy);
        this.myCpt = ko.observable(rstat.cpt);
        this.calls = ko.observable(rstat.calls);

        this.update = function (n) {
          this.occup(n.occupancy);
          this.myCpt(n.cpt);
          this.calls(n.calls);
        };
      };

      this.isExpanded = ko.observable(false);

      this.username = x.username;
      this.isSelf = oa.username() === x.username;

      this.update = function (info) {
        this.timer.reset();
        this.timer.start(Math.round((oa.getServerTime() - info.login_time) / 1000));

        this.first_name(info.first_name);
        this.last_name(info.last_name);
        this.jobTitle(info.job_title);
        this.avatar(info.avatar);
        this.location(info.location);
        this.localTime(info.localTime);
        this.profile(info.profile);
        this.node(info.reach_node);

        this.parseSkills(info.skills);
        this.parseState(info.state, info.active_channels);
      };

      this.monitor = function (toForceRelease) {
        if (oa.isProcessing()) {
          alert('Monitor not allowed while processing a call.');
          return;
        }

        oa.monitor(this.channelId(), this.username, toForceRelease, this.callJoinCallback, this);
      };

      this.barge = function (toForceRelease) {
        if (oa.isProcessing()) {
          alert('Barge not allowed while processing a call.');
          return;
        }

        oa.barge(this.channelId(), this.username, toForceRelease, this.callJoinCallback, this);
      };

      this.callJoinCallback = function (response) {
        // console.log('join', response)
        if (response.result && response.result.status === 'initiated') {
          switch (response.type) {
          case 'monitor_call':
            this.hasMonitorPending(true);
            break;
          case 'barge_call':
            this.hasBargePending(true);
            break;
          default:
            break;
          }
        }
        else if (response.error) {
          switch (response.error.code) {
          case '4003':
            confirm('Your status will be set to released. Click "OK" to proceed.', function (a) {
              if (a) {
                switch (response.type) {
                case 'monitor_call':
                  this.monitor(true);
                  break;
                case 'barge_call':
                  this.barge(true);
                  break;
                default:
                  break;
                }
              }
            }.bind(this));
            break;
          case '4005':
            alert('Unable to ring. Please check your phone connection');
            break;
          case '4007':
            break;
          default:
            // shouldn't happen unless something goes wrong
            alert('An error has occurred: ' + response.error.message);
            break;
          }
        }
      };

      this.updateRStat = function (info) {
        var entry = _.find(this.rstats(), function (rs) {
          return rs.coverage === info.coverage;
        });

        if (_.isObject(entry)) {
          entry.update(info);
        }
        else {
          this.rstats.push(new this.AgentRollingStat(info));
        }
      };

      this.parseSkills = function (skills) {
        this.skills(oa.filterBasicSkills(skills));
        this.csg(oa.filterCsgSkills(skills));
      };

      this.parseState = function (state, activeChannels) {
        if (_.isString(state)) {
          this.agentState('idle');
          this.releaseReason('');
          this.stateId(state);
          this.stateIdInput(state);
        }
        else if (_.isObject(state)) {
          this.agentState(_.keys(state)[0]);
          var v = _.values(state)[0];
          this.releaseReason(v.name);
          this.stateId(v.id);
          this.stateIdInput(v.id);
        }

        // get first active channel
        if (_.isArray(activeChannels) && _.isObject(activeChannels[0])) {
          this.media(activeChannels[0].type);
          this.channelId(activeChannels[0].channelid); // assume voice channel exists

          var acState = activeChannels[0].state;
          this.channelState(acState);
          if (this.isStateInSession()) {
            var acStateTimestamps = _.reduce(activeChannels[0].state_changes, function (memo, obj) {
              if (acState in obj) memo.push(obj[acState]);
              return memo;
            }, [], this);
            var acStateTimestamp = _.last(acStateTimestamps);
            this.inSessionSeconds(Math.round((oa.getServerTime() - acStateTimestamp) / 1000));
          }
          else {
            this.inSessionSeconds(0);
          }

          this.customer.callerid(activeChannels[0].callerid);
          oa.getClient(activeChannels[0].client, function (obj) {
            this.customer.name(obj.name);
            this.customer.avatar(obj.avatar);
          }, this);
        }
        else {
          this.channelState('');
        }
      };

      this.parseRStats = function (rstats) {
        var ars = _.map(rstats, function (rstat) {
          return new this.AgentRollingStat(rstat);
        }, this);

        this.rstats(ars);
      };

      this.tickInSessionSeconds = function () {
        if (!this.isStateInSession()) return;
        var currentSeconds = this.inSessionSeconds() + 1;
        this.inSessionSeconds(currentSeconds);
      };

      this.timer = new Timer();
      this.timer.start(Math.round((oa.getServerTime() - x.login_time) / 1000));
      this.loginDuration = ko.computed(function () {
        return this.toHHMM();
      }, this.timer);

      this.first_name = ko.observable(x.first_name || '');
      this.last_name = ko.observable(x.last_name || '');
      this.name = ko.computed(function () {
        var fname = this.first_name();
        var lname = this.last_name();

        fname = fname ? fname.trim() : '';
        lname = lname ? lname.trim() : '';

        return (fname + ' ' + lname).trim();
      }, this);
      this.jobTitle = ko.observable(x.job_title || '');
      this.avatar = ko.observable(x.avatar || '');
      this.location = ko.observable(x.location || '');
      this.localTime = ko.observable(x.localTime || '');
      this.profile = ko.observable(x.profile || '');
      this.node = ko.observable(x.reach_node);

      this.skills = ko.observableArray();
      this.csg = ko.observableArray();
      this.agentState = ko.observable();
      this.channelState = ko.observable();
      this.state = ko.computed(function () {
        var as = this.agentState();
        var cs = this.channelState();

        if (cs === '') {
          return as;
        }
        else {
          return cs;
        }
      }, this);
      this.releaseReason = ko.observable();
      this.stateId = ko.observable();
      this.stateIdInput = ko.observable();
      this.media = ko.observable();
      this.channelId = ko.observable();
      this.inSessionSeconds = ko.observable();
      this.timeInSession = ko.computed(function () {
        return Timer.prototype.utils.formatMMSS(Math.round(this.inSessionSeconds()));
      }, this);
      this.customer = {
        name: ko.observable(),
        callerid: ko.observable(),
        avatar: ko.observable(),
        isAvatarDefault: ko.observable(true)
      };

      this.rstats = ko.observableArray();
      this.rstatSelected = ko.computed(function () {
        return _.find(this.rstats(), function (obj) {
          return obj.coverage === statCoverage();
        }, this);
      }, this);
      this.occup = ko.computed(function () {
        var rstat = this.rstatSelected();
        if (!rstat) return '--';

        var occup = rstat.occup();
        if (!_.isFinite(occup)) return '--';

        return Math.round(occup) + '%';
      }, this);
      this.myCpt = ko.computed(function () {
        var rstat = this.rstatSelected();
        if (!rstat) return '--';

        var myCpt = rstat.myCpt();
        if (!_.isFinite(myCpt)) return '--';

        return Timer.prototype.utils.formatMMSS(Math.round(myCpt));
      }, this);
      this.calls = ko.computed(function () {
        var rstat = this.rstatSelected();
        if (!rstat) return '--';

        var calls = rstat.calls();
        if (!_.isFinite(calls)) return '--';

        return calls;
      }, this);

      this.jobTitleText = ko.computed(function () {
        var jobTitle = this.jobTitle();
        return jobTitle ? ', ' + jobTitle : '';
      }, this);
      this.skillsText = ko.computed(function () {
        return this.skills().join(', ');
      }, this);
      this.csgText = ko.computed(function () {
        return this.csg().join(', ');
      }, this);
      this.isStateReleased = ko.computed(function () {
        return this.state() === 'released';
      }, this);
      this.isStateIdle = ko.computed(function () {
        return this.state() === 'idle';
      }, this);
      this.isStatePrecall = ko.computed(function () {
        return this.state() === 'prering';
      }, this);
      this.isStateIncoming = ko.computed(function () {
        return this.state() === 'ringing';
      }, this);
      this.isStateInSession = ko.computed(function () {
        return this.state() === 'oncall';
      }, this);
      this.isStateWrapUp = ko.computed(function () {
        return this.state() === 'wrapup';
      }, this);
      this.isMediaVoice = ko.computed(function () {
        return this.media() === 'voice';
      }, this);
      this.isMediaVoicemail = ko.computed(function () {
        return this.media() === 'voicemail';
      }, this);
      this.isMediaEmail = ko.computed(function () {
        return this.media() === 'email';
      }, this);
      this.isProcessing = ko.computed(function () {
        return this.state() !== 'released' && this.state() !== 'idle';
      }, this);

      if(oa.permissionsMonitor() == true){
        this.isMonitorOn = ko.observable(this.username === oa.agentMonitorOn());
        this.hasMonitorPending = ko.observable(this.username === oa.agentMonitorPending());
        this.canMonitor = ko.computed(function () {
          return !this.isMonitorOn() && !this.hasMonitorPending();
        }, this);
      }
      else{
        this.isMonitorOn = ko.observable(false);
        this.hasMonitorPending = ko.observable(false);
        this.canMonitor = ko.observable(false);
      }
      if(oa.permissionsBarge() == true){
        this.isBargeOn = ko.observable(this.username === oa.agentBargeOn());
        this.hasBargePending = ko.observable(this.username === oa.agentBargePending());
        this.canBarge = ko.computed(function () {
          return !this.isBargeOn() && !this.hasBargePending();
        }, this);
      }
      else{
        this.isBargeOn = ko.observable(false);
        this.hasBargePending = ko.observable(false);
        this.canBarge = ko.observable(false);
      }

      this.parseSkills(x.skills);
      this.parseState(x.state, x.active_channels);
      if (x.rstats) {
        this.parseRStats(x.rstats.coverages);
      }

      this.stateId.subscribe(function (val) {
        this.stateIdInput(val); // sets value in select
      }, this);
      this.stateIdInput.subscribe(function (val) {
        if (val === this.stateId()) return;

        if (val === 'available') {
          oa.setAgentAvailable(this.username);
        }
        else if (val === 'kick') {
          if (this.isProcessing()) {
            notify('Cannot log off an agent processing a call.', 'warn', 5000);
            this.stateIdInput(this.stateId());
          }
          else {
            // allow kick if released or idle
            oa.kickAgent(this.username);
          }
        }
        else if (val !== 'default') {
          oa.setAgentReleased(this.username, val);
        }
        else {
          oa.setAgentReleased(this.username);
        }
      }, this);
      this.customer.avatar.subscribe(function () {
        // reset to show default avatar when avatar url is updated
        this.customer.isAvatarDefault(true);
      }, this);
    };

    this.Profile = function (x, statCoverage) {
      this.ProfileRollingStat = function (rstat) {
        this.coverage = rstat.coverage;
        this.occup = ko.observable(rstat.occupancy);
        this.cpt = ko.observable(rstat.cpt);
        this.calls = ko.observable(rstat.calls);

        this.update = function (n) {
          this.occup(n.occupancy);
          this.cpt(n.cpt);
          this.calls(n.calls);
        };
      };

      this.updateCounts = function (info) {
        if (!_.isObject(info)) return;

        this.total(info.total);
        this.released(info.released);
        this.idle(info.idle);
        this.ringing(info.ringing);
        this.inSession(info.oncall);
        this.wrapup(info.wrapup);
      };

      this.updateRStat = function (info) {
        var entry = _.find(this.rstats(), function (rs) {
          return rs.coverage === info.coverage;
        });

        if (_.isObject(entry)) {
          entry.update(info);
        }
        else {
          this.rstats.push(new this.ProfileRollingStat(info));
        }
      };

      this.parseSkills = function (skills) {
        this.skills(oa.filterBasicSkills(skills).concat(oa.filterCsgSkills(skills)));
      };

      this.parseRStats = function (rstats) {
        var prs = _.map(rstats, function (rstat) {
          return new this.ProfileRollingStat(rstat);
        }, this);

        this.rstats(prs);
      };

      this.name = x.name;

      this.skills = ko.observableArray();

      this.skillsText = ko.computed(function () {
        return this.skills().join(', ');
      }, this);

      this.rstats = ko.observableArray();
      this.rstatSelected = ko.computed(function () {
        return _.find(this.rstats(), function (obj) {
          return obj.coverage === statCoverage();
        }, this);
      }, this);
      this.occup = ko.computed(function () {
        var rstat = this.rstatSelected();
        if (!rstat) return '--';

        var occup = rstat.occup();
        if (!_.isFinite(occup)) return '--';

        return Math.round(occup) + '%';
      }, this);
      this.cpt = ko.computed(function () {
        var rstat = this.rstatSelected();
        if (!rstat) return '--';

        var cpt = rstat.cpt();
        if (!_.isFinite(cpt)) return '--';

        return Timer.prototype.utils.formatMMSS(Math.round(cpt));
      }, this);
      this.calls = ko.computed(function () {
        var rstat = this.rstatSelected();
        if (!rstat) return '--';

        var calls = rstat.calls();
        if (!_.isFinite(calls)) return '--';

        return calls;
      }, this);

      this.total = ko.observable(0);
      this.released = ko.observable(0);
      this.idle = ko.observable(0);
      this.ringing = ko.observable(0);
      this.inSession = ko.observable(0);
      this.wrapup = ko.observable(0);

      this.parseSkills(x.skills);
      this.parseRStats(x.rstats);
      if (_.isObject(x.counts)) {
        this.updateCounts(x.counts);
      }
    };

    this.onCallJoinEvent = function (type, d) {
      // type = barge/monitor
      var entry = _.find(this.agents(), function (a) {
        return a.username === d.agent;
      });
      if (!_.isObject(entry)) return;

      var hasPending = entry['has' + $.oucUtils.capitalize(type) + 'Pending'];
      if (ko.isObservable(hasPending)) {
        hasPending(false);
      }

      // console.log(type, entry);
      var isOn = entry['is' + $.oucUtils.capitalize(type) + 'On'];
      if (d.status === 'success') {
        if (entry && entry.channelId() === d.channelid) {
          if (ko.isObservable(isOn)) {
            isOn(true);
          }
        }
      }
      else if (d.status === 'ended') {
        if (entry && entry.channelId() === d.channelid) {
          if (ko.isObservable(isOn)) {
            isOn(false);
          }
        }
      }
      else if (d.status === 'error') {
        switch (d.data.code) {
        case '4001':
          alert('Unable to ring. Please check your phone connection');
          break;
        case '4006':
          break;
        default:
          // shouldn't happen unless something goes wrong
          alert('An error has occurred: ' + d.data.message);
          break;
        }
      }
    };

    this.initialize = function () {
      amplify.subscribe(oa.events.RECEIVEDPROFILECOUNT, this, function (d) {
        var pushList = [];
        var removeList = [];

        _.each(d.data, function (val, name) {
          var entry = _.find(this.profiles(), function (p) {
            return p.name === name;
          });

          if (_.isObject(val)) {
            if (_.isObject(entry)) {
              entry.updateCounts(val);
            }
            else {
              var match = _.contains(oa.permissionsAgents() ,name);
              if(match === true){
                var newEntry = new this.Profile({
                  name: name
                }, this.profileStatsCoverage);
                newEntry.updateCounts(val);
                pushList.push(newEntry);
              }
            }
          }
          else if (val === 'deleted') {
            removeList.push(entry);
          }
        }, this);

        if (removeList.length > 0) {
          this.profiles.removeAll(removeList);
        }

        if (pushList.length > 0) {
          this.profiles.push.apply(this.profiles, pushList);
        }
      });

      amplify.subscribe(oa.events.UPDATEDPROFILERSTAT, this, function (d) {
        _.each(d.data.updates, function (val) {
          var entry = _.find(this.profiles(), function (p) {
            return p.name === val.profile;
          });

          if (_.isObject(entry)) {
            entry.updateRStat(val);
          }
        }, this);
      });

      amplify.subscribe(oa.events.UPDATEDAGENTS, this, function (d) {
        var pushList = [];
        var removeList = [];

        _.each(d.data, function (n, u) {
          var entry = _.find(this.agents(), function (a) {
            return a.username === u;
          });

          if (_.isObject(n)) {
            if (_.isObject(entry)) {
              entry.update(n);
            }
            else {
              var match = _.contains(oa.permissionsAgents() ,n.profile);
              if(match === true){
                n['username'] = u;
                pushList.push(new this.Agent(n, this.agentStatsCoverage));
              }
            }
          }
          else if (n === 'offline') {
            removeList.push(entry);
          }
        }, this);

        if (removeList.length > 0) {
          this.agents.removeAll(removeList);
        }

        if (pushList.length > 0) {
          this.agents.push.apply(this.agents, pushList);
          this.sortAgentsBy(null, true);
        }
      });

      amplify.subscribe(oa.events.UPDATEDAGENTRSTAT, this, function (d) {
        _.each(d.data.updates, function (val) {
          var entry = _.find(this.agents(), function (a) {
            return a.username === val.username;
          });

          if (_.isObject(entry)) {
            entry.updateRStat(val);
          }
        }, this);
      });

      amplify.subscribe(oa.events.RECEIVEDCALLMONITORRESULT, this, function (d) {
        // console.log('monitor', d)
        this.onCallJoinEvent('monitor', d);
      });

      amplify.subscribe(oa.events.RECEIVEDCALLMONITORENDED, this, function (d) {
        // console.log('monitor', d)
        d.status = 'ended';
        this.onCallJoinEvent('monitor', d);
      });

      amplify.subscribe(oa.events.RECEIVEDCALLBARGERESULT, this, function (d) {
        // console.log('barge', d)
        this.onCallJoinEvent('barge', d);
      });

      amplify.subscribe(oa.events.RECEIVEDCALLBARGEENDED, this, function (d) {
        // console.log('barge', d)
        d.status = 'ended';
        this.onCallJoinEvent('barge', d);
      });

      this.fetchInit();
    };

    this.reset = function () {
      this.profiles([]);
      this.profileStatsCoverage('last_hr');

      this.agents([]);
      this.nameFilterBuffer('');
      this.profileFilter('');
      this.customerFilter('');
      this.stateFilter('');
      this.skillFilter('');
      this.nodeFilter('');
      this.agentStatsCoverage('last_hr');

      this.agentsSortIsAscendingMap.name(true);
      this.agentsSortIsAscendingMap.occup(true);
      this.agentsSortIsAscendingMap.calls(true);
      this.agentsSortIsAscendingMap.myCpt(true);
      this.agentsSortIsAscendingMap.loginDuration(true);
      this.agentsSortIsAscendingMap.state(true);

      this.agentsSortedBy('name');

      this.fetchInit();
    };

    this.fetchInit = function () {
      oa.getClients();
      oa.getSkills();
      oa.getNodes();
      oa.getReleaseOptions();

      oa.subscribeAgents(function (result) {
        var agents = _.filter(result, function (x) {
          var match = _.contains(oa.permissionsAgents() ,x.profile);
          if(match === true){
            return x;
          }
        }, this);

        agents = _.map(agents, function (x) {
          return new this.Agent(x, this.agentStatsCoverage);
        }, this);

        this.agents(agents);
        this.sortAgentsBy(null);

        // unified timer for calls in session
        var self = this;
        if (this.intervalId) clearInterval(this.intervalId);
        this.intervalId = setInterval(function () {
          _.each(self.agents(), function (agent) {
            agent.tickInSessionSeconds();
          });
        }, 1000);
      }, this);

      // agent profiles
      oa.subscribeAgentProfiles(function (result) {
        var profiles = _.filter(result, function (x) {
          var match = _.contains(oa.permissionsAgents() ,x.name);
          if(match === true){
            return x;
          }
        }, this);

        profiles = _.map(profiles, function (x) {
          return new this.Profile(x, this.profileStatsCoverage);
        }, this);

        this.profiles(profiles);
      }, this);
    };
  };

  var sortByBinding = function (key, vm) {
    return {
      attr: {
        title: vm.sortByIconTitle(key)
      },
      css: {
        'active': vm.isSortedBy(key)
      },
      event: {
        click: function () {
          vm.sortAgentsBy(key);
        }
      }
    };
  };
  var agentFilterBgBinding = function (valueBind) {
    if (!ko.isObservable(valueBind)) return {};

    return {
      css: {
        'active': Boolean(valueBind())
      }
    };
  };
  var bindings = {
    amProfileStatsCoverage: function () {
      return {
        options: this.availableCoverage,
        optionsText: 'name',
        optionsValue: 'apiText',
        value: this.profileStatsCoverage
      };
    },
    amProfiles: function () {
      return {
        foreach: this.profiles
      };
    },
    amProfileName: function () {
      return {
        text: this.name
      };
    },
    amProfileSkillsText: function () {
      return {
        text: this.skillsText
      };
    },
    amProfileAgentsTotal: function () {
      return {
        flashingText: this.total
      };
    },
    amProfileAgentsReleased: function () {
      return {
        flashingText: this.released
      };
    },
    amProfileAgentsIdle: function () {
      return {
        flashingText: this.idle
      };
    },
    amProfileAgentsRinging: function () {
      return {
        flashingText: this.ringing
      };
    },
    amProfileAgentsInSession: function () {
      return {
        flashingText: this.inSession
      };
    },
    amProfileAgentsWrapup: function () {
      return {
        flashingText: this.wrapup
      };
    },
    amProfileStatsOccup: function () {
      return {
        flashingText: this.occup
      };
    },
    amProfileStatsCpt: function () {
      return {
        flashingText: this.cpt
      };
    },
    amProfileStatsCalls: function () {
      return {
        flashingText: this.calls
      };
    },
    amAgents: function () {
      return {
        foreach: this.filteredAgents
      };
    },
    amAgentNoMatchMessage: function () {
      return {
        'if': this.filteredAgents().length === 0
      };
    },
    amAgentSortArrowsRow: function () {
      return {
        'if': this.filteredAgents().length > 0
      };
    },
    amAgentRow: function () {
      return {
        attr: {
          id: 'agent-' + this.username
        },
        css: {
          'bg-lightgray expanded': this.isExpanded,
          'bg-lightblue': !this.isExpanded()
        }
      };
    },
    amAgentMoreInfoIcon: function () {
      return {
        event: {
          click: function () {
            this.isExpanded(!this.isExpanded());
          }
        },
        css : {
          'icon-arrow-down': this.isExpanded,
          'icon-arrow-right': !this.isExpanded()
        }
      };
    },
    amAgentMoreInfo: function () {
      return {
        visible: this.isExpanded
      };
    },
    amAgentDetailsCell: function () {
      return {
        attr: {
          rowspan: this.isExpanded() ? 2 : 1
        }
      };
    },
    amAgentPic: function () {
      return {
        attr: {
          src: this.avatar
        },
        event: {
          load: function (vm, e) {
            $(e.currentTarget).parent().css('visibility', 'visible');
          },
          error: function (vm, e) {
            $(e.currentTarget).hide();
            $(e.currentTarget).parent().css('visibility', 'visible');
          }
        }
      };
    },
    amAgentPicStatus: function () {

    },
    amAgentName: function () {
      return {
        text: this.name
      };
    },
    amAgentUsername: function () {
      return {
        text: ' ' + this.username
      };
    },
    amAgentJobTitle: function () {
      return {
        text: this.jobTitleText
      };
    },
    amAgentProfile: function () {
      return {
        text: this.profile,
        textNone: true
      };
    },
    amAgentSkills: function () {
      return {
        text: this.skillsText,
        textNone: true
      };
    },
    amAgentCsg: function () {
      return {
        text: this.csgText,
        textNone: true
      };
    },
    amAgentLocation: function () {
      return {
        text: this.location
      };
    },
    amAgentLocalTime: function () {
      return {
        text: this.localTime
      };
    },
    amAgentOccup: function () {
      return {
        text: this.occup
      };
    },
    amAgentCpt: function () {
      return {
        text: this.myCpt
      };
    },
    amAgentCalls: function () {
      return {
        text: this.calls
      };
    },
    amAgentLoginDuration: function () {
      return {
        text: this.loginDuration
      };
    },
    amAgentState: function () {
      return {
        css: {
          'released': this.isStateReleased,
          'idle': this.isStateIdle,
          'precall': this.isStatePrecall,
          'incoming': this.isStateIncoming,
          'session': this.isStateInSession,
          'wrap': this.isStateWrapUp
        }
      };
    },
    amAgentMedia: function () {
      return {
        css: {
          'session-call': this.isMediaVoice,
          'session-voicemail': this.isMediaVoicemail,
          'session-email': this.isMediaEmail
        }
      };
    },
    amAgentSessionTimer: function () {
      return {
        text: this.timeInSession
      };
    },
    amAgentCustomerName: function () {
      return {
        text: this.customer.name
      };
    },
    amAgentCustomerCallerId: function () {
      return {
        text: this.customer.callerid
      };
    },
    amAgentCustomerAvatar: function () {
      return {
        visible: !this.customer.isAvatarDefault(),
        attr: {
          src: this.customer.avatar,
        },
        event: {
          load: function (vm, e) {
            this.customer.isAvatarDefault(false);
          },
          error: function (vm, e) {
            this.customer.isAvatarDefault(true);
          }
        }
      };
    },
    amAgentCustomerAvatarDefault: function () {
      // default avatar is set as background
      // if customer has a valid avatar, set background to none
      return {
        style: {
          'background': this.customer.isAvatarDefault() ? '' : 'none'
        }
      };
    },
    amAgentReleaseReason: function () {
      return {
        text: this.releaseReason
      };
    },
    amAgentProfileMenu: function () {
      return {
        attr: {
          id: 'agents-table-profile-menu-' + this.username
        }
      };
    },
    amAgentStateMenu: function () {
      return {
        attr: {
          id: 'agents-table-status-menu-' + this.username
        }
      };
    },
    amAgentStateBg: function () {
      return {
        css: {
          'bg-lightgreen': this.isStateInSession(),
          'bg-lightblue': this.isStateReleased(),
          'bg-orange': !this.isStateInSession() && !this.isStateReleased()
        }
      };
    },
    amAgentVisibleWhenReleased: function () {
      return {
        visible: this.isStateReleased
      };
    },
    amAgentVisibleWhenProcessing: function () {
      return {
        visible: this.isProcessing
      };
    },
    amAgentVisibleWhenInSession: function () {
      return {
        visible: this.isStateInSession
      };
    },
    amAgentVisibleWhenJoinable: function () {
      return {
        visible: this.isStateInSession() && !this.isSelf
      };
    },
    amAgentNameFilter: function () {
      return {
        value: this.nameFilterBuffer,
        valueUpdate: 'keyup'
      };
    },
    amAgentProfileFilter: function () {
      return {
        options: this.profileList,
        value: this.profileFilter,
        optionsCaption: 'Any Profile'
      };
    },
    amAgentProfileFilterBg: function () {
      return agentFilterBgBinding(this.profileFilter);
    },
    amAgentCustomerFilter: function () {
      return {
        options: this.customerList,
        value: this.customerFilter,
        optionsCaption: 'Any Customer'
      };
    },
    amAgentCustomerFilterBg: function () {
      return agentFilterBgBinding(this.customerFilter);
    },
    amAgentStateFilter: function () {
      return {
        options: this.states,
        optionsText: 'name',
        optionsValue: 'apiText',
        value: this.stateFilter,
        optionsCaption: 'Any Agent State'
      };
    },
    amAgentStateFilterBg: function () {
      return agentFilterBgBinding(this.stateFilter);
    },
    amAgentSkillFilter: function () {
      return {
        options: this.skillList,
        value: this.skillFilter,
        optionsCaption: 'Any Skill'
      };
    },
    amAgentSkillFilterBg: function () {
      return agentFilterBgBinding(this.skillFilter);
    },
    amAgentNodeFilter: function () {
      return {
        options: this.nodeList,
        value: this.nodeFilter,
        optionsCaption: 'Any Node'
      };
    },
    amAgentNodeFilterBg: function () {
      return agentFilterBgBinding(this.nodeFilter);
    },
    amAgentSupportGroupFilter: function () {
      return {
      };
    },
    amAgentSupportGroupFilterBg: function () {
      return agentFilterBgBinding();
    },
    amAgentStatsCoverage: function () {
      return {
        options: this.availableCoverage,
        optionsText: 'name',
        optionsValue: 'apiText',
        value: this.agentStatsCoverage,
        optionsCaption: 'Select Time Interval...'
      };
    },
    amAgentStatsCoverageBg: function () {
      return agentFilterBgBinding(this.agentStatsCoverage);
    },
    amAgentSortByName: function (ctx) {
      return sortByBinding('name', ctx.$root);
    },
    amAgentSortByOccup: function (ctx) {
      return sortByBinding('occup', ctx.$root);
    },
    amAgentSortByMyCpt: function (ctx) {
      return sortByBinding('myCpt', ctx.$root);
    },
    amAgentSortByCalls: function (ctx) {
      return sortByBinding('calls', ctx.$root);
    },
    amAgentSortByLoginDuration: function (ctx) {
      return sortByBinding('loginDuration', ctx.$root);
    },
    amAgentSortByState: function (ctx) {
      return sortByBinding('state', ctx.$root);
    },
    // expanded agent options (inside foreach: agents)
    // agent-state-menu dropdown
    amAgentReleaseDropdown: function (ctx) {
      return {
        enable: oa.permissionsControlAgentState(),
        value: this.stateIdInput,
        options: ctx.$root.releaseReasons,
        optionsText: 'name',
        optionsValue: 'id'
      };
    },
    amAgentMonitor: function () {
      return {
        click: function () {
          this.monitor(false);
        },
        enable: this.canMonitor,
        css: {
          'active': this.isMonitorOn()
        }
      };
    },
    amAgentBarge: function () {
      return {
        click: function () {
          this.barge(false);
        },
        enable: this.canBarge,
        css: {
          'active': this.isBargeOn()
        }
      };
    }
  };

  ko.bindingProvider.instance.registerBindings(bindings);

  return {
    ViewModel: AgentManagerVM,
    Parent: Parent,
    meta: meta,
    template: _.template(template, {})
  };
});
