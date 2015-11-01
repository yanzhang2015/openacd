define([
  'knockout',
  'models/timer',
  'models/gadget',
  'models/openacd',
  'json!../meta/my-statistics.json',
  'text!../templates/my-statistics.html'
], function (ko, Timer, Parent, oa, meta, template) {
  var MyStatisticsVM = function () {
    this.RollingStat = function (x) {
      this.Duration = function (y) {
        this.update = function (n) {
          this.cpt(n.cpt);
          this.myCpt(n.my_cpt);
          this.occup(n.occupancy);
          this.asa(n.average_wait_duration);
          this.longest(n.longest_call_duration);
        };

        this.duration = y.duration;
        this.cpt = ko.observable();
        this.myCpt = ko.observable();
        this.occup = ko.observable();
        this.asa = ko.observable();
        this.longest = ko.observable();

        this.update(y.data);
      };

      this.updateDuration = function (d) {
        var entry = this.getDuration(d.duration);

        if (_.isObject(entry)) {
          entry.update(d.data);
        }
        else {
          this.durations.push(new this.Duration(d));
        }
      };

      this.getDuration = function (duration) {
        return _.findWhere(this.durations(), {duration: duration});
      };

      this.name = x.client;
      this.durations = ko.observableArray();
      var rstats = _.map(x.rstats, function (rstat) {
        return new this.Duration(rstat);
      }, this);
      this.durations(rstats);
    };

    this.LiveStat = function (x) {
      this.update = function (n) {
        this.ciq(n.calls_in_queue);

        var c = n.agent_count;

        if (_.isObject(c.idle)) {
          this.released(c.idle.released);
          this.idle(c.idle.available);
        }

        if (_.isObject(c.ringing)) {
          this.ringing(c.ringing.available + c.ringing.released);
        }

        if (_.isObject(c.oncall)) {
          this.inSession(c.oncall.available + c.oncall.released);
        }

        if (_.isObject(c.wrapup)) {
          this.wrapup(c.wrapup.available + c.wrapup.released);
        }
      };

      this.name = x.name;

      this.ciq = ko.observable();
      this.released = ko.observable();
      this.idle = ko.observable();
      this.inSession = ko.observable();
      this.wrapup = ko.observable();
      this.ringing = ko.observable();
      this.agents = ko.computed(function () {
        return this.released() + this.idle() + this.inSession() + this.wrapup() + this.ringing();
      }, this);

      this.update(x.stats);
    };

    this.updateRollingStats = function () {
      oa.getMyRollingStats(function (stats) {
        var rolling = [];
        _.each(stats.clients, function (customerRStats) {
          var c = customerRStats.client;

          // check if supported by user
          if (_.contains(this.customers(), c)) {
            rolling.push(new this.RollingStat(customerRStats));
          }
        }, this);
        this.rollingClientStats(rolling);

        this.rollingStatsAll(new this.RollingStat({
          client: 'all',
          rstats: stats.all
        }));
      }, this);
    };

    this.updateLiveStats = function () {
      oa.getLiveStats(function (stats) {
        var live = _.map(stats.clients, function (stat) {
          return new this.LiveStat(stat);
        }, this);
        this.liveClientStats(live);

        var total = new this.LiveStat({
          name: 'total',
          stats: {
            calls_in_queue: stats.total_calls_in_queue,
            agent_count: stats.total_agent_count
          }
        });
        this.liveStatsTotal(total);

        var self = this;
        setTimeout(function () {
          self.updateLiveStats();
        }, 5000);
      }, this);
    };

    this.initialize = function () {
      amplify.subscribe(oa.events.UPDATEDMYROLLINGSTATS, this, function (d) {
        var rolling = [];

        _.each(d.data.clients, function (ncs) {
          var entry = _.find(this.rollingClientStats(), function (rcs) {
            return rcs.name === ncs.client;
          });

          if (_.isObject(entry)) {
            _.each(ncs.rstats, function (val) {
              entry.updateDuration(val);
            });
          }
          else {
            var c = ncs.client;

            // check if supported by user
            if (_.contains(this.customers(), c)) {
              rolling.push(new this.RollingStat(ncs));
            }
          }
        }, this);

        if (rolling.length > 0) {
          this.rollingClientStats.push.apply(this.rollingClientStats, rolling);
        }

        _.each(d.data.all, function (val) {
          this.rollingStatsAll().updateDuration(val);
        }, this);
      });

      this.fetchInit();
    };

    this.reset = function () {
      this.customer('');
      this.duration('last_hr');

      this.media('voice');

      this.rollingClientStats([]);
      this.rollingStatsAll(new this.RollingStat({
        client: 'all',
        rstats: _.map(this.durations(), function (d) {
          return {
            'duration': d.apiText,
            'data': {}
          };
        }, this)
      }), this);
      this.liveClientStats([]);
      this.liveStatsTotal(new this.LiveStat({
        name: 'total',
        stats: {
          'calls_in_queue': 0,
          'agent_count': 0
        }
      }));

      this.fetchInit();
    };

    this.fetchInit = function () {
      oa.getConnectionInfo();
      oa.getMyRollingStats();
      this.updateLiveStats();
    };

    this.customers = ko.computed(function () {
      var connInfo = oa.connectionInfo();
      if (!connInfo) return [];
      var clients = [];
      _.each(connInfo.skills, function (sk) {
        if (_.isObject(sk)) {
          if (_.isString(sk.client)) {
            clients.push(sk.client);
          }
        }
      });

      return clients.sort();
    });
    this.customer = ko.observable('');
    this.durations = ko.computed(function () {
      return oa.rStatsCoverageList();
    });
    this.duration = ko.observable('last_hr');

    this.media = ko.observable('voice');
    this.profileName = ko.computed(function () {
      var connInfo = oa.connectionInfo();
      return connInfo ? connInfo.profile : '';
    });
    this.skills = ko.computed(function () {
      var connInfo = oa.connectionInfo();
      if (!connInfo) return [];
      return oa.filterBasicSkills(connInfo.skills);
    });
    this.skillsText = ko.computed(function () {
      return this.skills().join(', ');
    }, this);

    this.rollingClientStats = ko.observableArray();
    this.rollingStatsAll = ko.observable(new this.RollingStat({
      client: 'all',
      rstats: _.map(this.durations(), function (d) {
        return {
          'duration': d.apiText,
          'data': {}
        };
      }, this)
    }), this);
    this.liveClientStats = ko.observableArray();
    this.liveStatsTotal = ko.observable(new this.LiveStat({
      name: 'total',
      stats: {
        'calls_in_queue': 0,
        'agent_count': 0
      }
    }));

    // computed observables for display
    this.rollingStatDisplay = ko.computed(function () {
      var d = this.duration();
      var c = this.customer();

      var rcs = _.where(this.rollingClientStats(), {name: c})[0];
      if (_.isObject(rcs)) {
        return rcs.getDuration(d);
      }
      else {
        return this.rollingStatsAll().getDuration(d);
      }
    }, this);
    this.rollingStatMMSS = function (key) {
      return ko.computed(function () {
        var rs = this.rollingStatDisplay();

        if (_.isObject(rs)) {
          var n = rs[key]();
          if (_.isFinite(n)) {
            return Timer.prototype.utils.formatMMSS(Math.round(n));
          }
        }

        return '--';
      }, this);
    };
    this.cptText = this.rollingStatMMSS('cpt');
    this.myCptText = this.rollingStatMMSS('myCpt');
    this.asaText = this.rollingStatMMSS('asa');
    this.longestText = this.rollingStatMMSS('longest');
    this.occupText = ko.computed(function () {
      var rs = this.rollingStatDisplay();

      if (_.isObject(rs)) {
        var o = rs.occup();
        if (_.isFinite(o)) {
          return Math.round(o) + '%';
        }
      }

      return '--';
    }, this);
    this.cToVmText = ko.observable('--');
    this.liveStatDisplay = ko.computed(function () {
      var c = this.customer();

      var lcs = _.where(this.liveClientStats(), {name: c})[0];
      if (_.isObject(lcs)) {
        return lcs;
      }
      else {
        return this.liveStatsTotal();
      }
    }, this);
    this.liveStatBarWidth = function (key) {
      return ko.computed(function () {
        var lcs = this.liveStatDisplay();
        if (!_.isObject(lcs)) return '0';

        var val = lcs[key]();
        var agents = lcs.agents();

        if (!_.isFinite(val) || !_.isFinite(agents)) return '0';

        var percent = Math.round(val / agents * 100);
        return percent + '%';
      }, this);
    };
    this.ciqText = ko.computed(function () {
      var dep = this.liveStatDisplay();
      if (!dep) return '--';

      var ciq = dep.ciq();
      if (!_.isFinite(ciq)) return '--';

      return ciq;
    }, this);
    this.agentsText = ko.computed(function () {
      var dep = this.liveStatDisplay();
      if (!dep) return '--';

      var agents = dep.agents();
      if (!_.isFinite(agents)) return '--';

      return agents;
    }, this);
    this.releasedBarWidth = this.liveStatBarWidth('released');
    this.idleBarWidth = this.liveStatBarWidth('idle');
    this.inSessionBarWidth = this.liveStatBarWidth('inSession');
    this.wrapupBarWidth = this.liveStatBarWidth('wrapup');
    this.ringingBarWidth = this.liveStatBarWidth('ringing');
  };

  var mediaButtonBinding = function (key, vm) {
    return {
      css: {
        'active': vm.media() === key
      },
      event: {
        click: function () {
          vm.media(key);
        }
      }
    };
  };
  var bindings = {
    msMediaVoice: function () {
      return mediaButtonBinding('voice', this);
    },
    msMediaEmail: function () {
      return mediaButtonBinding('email', this);
    },
    msMediaVoicemail: function () {
      return mediaButtonBinding('voicemail', this);
    },
    msProfileName: function () {
      return {
        text: this.profileName,
        textNone: true
      };
    },
    msSkills: function () {
      return {
        text: this.skillsText,
        textNone: true
      };
    },
    msCiqSelected: function () {
      return {
        text: this.ciqText
      };
    },
    msCiqLeft: function () {
      return {
        text: '--'
      };
    },
    msCiqRight: function () {
      return {
        text: '--'
      };
    },
    msAgents: function () {
      return {
        text: this.agentsText
      };
    },
    msReleasedBar: function () {
      return {
        style: {
          width: this.releasedBarWidth
        }
      };
    },
    msIdleBar: function () {
      return {
        style: {
          width: this.idleBarWidth
        }
      };
    },
    msInSessionBar: function () {
      return {
        style: {
          width: this.inSessionBarWidth
        }
      };
    },
    msWrapupBar: function () {
      return {
        style: {
          width: this.wrapupBarWidth
        }
      };
    },
    msRingingBar: function () {
      return {
        style: {
          width: this.ringingBarWidth
        }
      };
    },
    msMyCpt: function () {
      return {
        text: this.myCptText
      };
    },
    msCpt: function () {
      return {
        text: this.cptText
      };
    },
    msOccup: function () {
      return {
        text: this.occupText
      };
    },
    msAsa: function () {
      return {
        text: this.asaText
      };
    },
    msLongest: function () {
      return {
        text: this.longestText
      };
    },
    msCToVm: function () {
      return {
        text: this.cToVmText
      };
    },
    msCustomerDropdown: function () {
      return {
        options: this.customers,
        optionsCaption: 'All Customers',
        value: this.customer
      };
    },
    msDurationDropdown: function () {
      return {
        options: this.durations,
        optionsText: 'name',
        optionsValue: 'apiText',
        value: this.duration
      };
    }
  };

  ko.bindingProvider.instance.registerBindings(bindings);

  return {
    ViewModel: MyStatisticsVM,
    Parent: Parent,
    meta: meta,
    template: _.template(template, {})
  };
});