define([
  'knockout',
  'models/timer',
  'models/gadget',
  'models/openacd',
  'json!../meta/queue-manager.json',
  'text!../templates/queue-manager.html'
], function (ko, Timer, Parent, oa, meta, template) {
  var QueueManagerVM = function () {
    //if(oa.permissionsQueueManager() == false) {
    //  return;
    //}
    this.coverageList = ko.computed(function () {
      return oa.rStatsCoverageList();
    });

    // Queues section
    this.QueueInfo = function (x, statCoverage) {
      this.name = x.name;
      this.media = ko.observable();
      this.isExpanded = ko.observable(false);
      this.lines = ko.observableArray();
      this.ciq = ko.observable();
      this.expectedWait = ko.observable();
      this.connected = ko.observable();

      this.rstats = ko.observableArray();
      this.rstatSelected = ko.computed(function () {
        return _.find(this.rstats(), function (obj) {
          return obj.coverage === statCoverage();
        }, this);
      }, this);

      this.Line = function (y) {
        this.ext = y.extension;
        this.didN = y.did_number;
        this.customerName = y.client;
      };

      this.QueueRollingStat = function (y) {
        this.coverage = y.coverage;
        this.aAnswerSeconds = ko.observable();
        this.aSpeedAnswer = ko.computed(function () {
          var aas = this.aAnswerSeconds();
          if (!_.isFinite(aas)) return '--';

          return Timer.prototype.utils.formatMMSS(Math.round(aas));
        }, this);
        this.maxWaitSeconds = ko.observable();
        this.longestWait = ko.computed(function () {
          var mw = this.maxWaitSeconds();
          if (!_.isFinite(mw)) return '--';

          return Timer.prototype.utils.formatMMSS(Math.round(mw));
        }, this);
        this.completed = ko.observable();
        this.aAbandonSeconds = ko.observable();
        this.aAbandonTime = ko.computed(function () {
          var aad = this.aAbandonSeconds();
          if (!_.isFinite(aad)) return '--';

          return Timer.prototype.utils.formatMMSS(Math.round(aad));
        }, this);
        this.abandoned = ko.observable();

        this.update = function (n) {
          this.aAnswerSeconds(n.average_wait_duration);
          this.maxWaitSeconds(n.max_wait_duration);
          this.completed(n.calls || 0);
          this.aAbandonSeconds(n.average_abandon_duration);
          this.abandoned(n.abandoned_calls || 0);
        };

        this.update(y);
      };

      this.update = function (n) {
        // assume everything is voice for now
        this.media('voice');
        this.ciq(n.calls_in_queue);
        this.connected(n.calls_connected);
        this.expectedWait('--');

        if (n.lines) {
          var lines = _.map(n.lines, function (line) {
            return new this.Line(line);
          }, this);
          this.lines(_.sortBy(lines, 'ext'));
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
          this.rstats.push(new this.QueueRollingStat(info));
        }
      };

      this.parseRStats = function (rstats) {
        var qrs = _.map(rstats, function (rstat) {
          return new this.QueueRollingStat(rstat);
        }, this);

        this.rstats(qrs);
      };

      this.isMediaMatch = function (media) {
        if (!media) return true;

        return this.media().toLowerCase() === media.toLowerCase();
      };

      this.update(x);
      this.parseRStats(x.rstats);
    };

    this.queues = ko.observableArray(); // get_queues
    this.qRollingStatsCoverage = ko.observable('last_hr');
    this.queueMediaFilter = ko.observable('voice');
    this.filteredQueues = ko.computed(function () {
      return _.filter(this.queues(), function (queue) {
        return queue.isMediaMatch(this.queueMediaFilter());
      }, this);
    }, this).extend({throttle: 300}); // throttle this to optimize animations
    // end Queues section

    // Calls section
    this.Call = function (x) {
      this.id = x.id;
      this.isExpanded = ko.observable(false);
      this.queue = ko.observable();
      this.media = ko.observable();
      this.customerName = ko.observable();
      this.customerCallerId = ko.observable();
      this.customerAvatar = ko.observable();
      this.line = ko.observable();
      this.skillsRequired = ko.observableArray();
      this.skillsReq = ko.computed(function () {
        return this.skillsRequired().join(', ');
      }, this);
      this.node = ko.observable();
      this.agMatch = ko.observable();

      this.queuedSeconds = ko.observable();
      this.tInQueue = ko.computed(function () {
        return Timer.prototype.utils.formatMMSS(Math.round(this.queuedSeconds()));
      }, this);

      this.waitSeconds = ko.observable();
      this.expecWait = ko.computed(function () {
        return Timer.prototype.utils.formatMMSS(Math.round(this.waitSeconds()));
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

      this.isFieldMatch = function (key, value) {
        if (!value) return true;

        if (key === 'skillsReq') return this.isSkillRequired(value);
        if (key === 'media') return this.isMediaMatch(value);

        if (!this[key]()) return true;

        return this[key]().toLowerCase() === value.toLowerCase();
      };

      this.isSkillRequired = function (skill) {
        var skillsLower = _.map(this.skillsRequired(), function (sk) {
          return sk.toLowerCase();
        }, this);

        return _.contains(skillsLower, skill.toLowerCase());
      };

      this.isMediaMatch = function (mediaList) {
        return _.contains(mediaList, this.media());
      };

      this.parseSkills = function (skills) {
        this.skillsRequired([]);

        _.each(skills, function (sk) {
          if (_.isString(sk)) {
            // magic if it starts with _
            if (sk.indexOf('_') !== 0) {
              this.skillsRequired.push(sk);
            }
          }
          else if (_.isObject(sk)) {
            var key = _.keys(sk)[0];
            if (key === 'agent') {
              this.skillsRequired.push('Agent: ' + sk[key]);
            }
          }
        }, this);
      };

      this.tickQueuedSeconds = function () {
        var currentSeconds = this.queuedSeconds() + 1;
        this.queuedSeconds(currentSeconds);
      };

      this.update = function (n) {
        this.queue(n.queue);
        this.customerCallerId(n.callerid);

        if (n.source_module === 'freeswitch_voicemail') {
          this.media('voicemail');
        }
        else {
          this.media('voice');
        }

        oa.getClient(n.client, function (obj) {
          this.customerName(obj.name);
          this.customerAvatar(obj.avatar);
        }, this);

        this.line(n.line);
        this.parseSkills(n.skills);
        this.node(n.reach_node);
        this.agMatch(n.available_agent_count);

        this.queuedSeconds(Math.round((oa.getServerTime() - n.time_queued) / 1000));

        this.waitSeconds(0);
      };

      this.update(x);
    };


    this.calls = ko.observableArray();
    this.callQueueFilter = ko.observable();
    this.callMediaFilter = ko.observableArray(['voice', 'voicemail']);
    this.callLineFilter = ko.observable();
    this.callCustomerFilter = ko.observable();
    this.callSkillFilter = ko.observable();
    this.callNodeFilter = ko.observable();

    this.filteredCalls = ko.computed(function () {
      return _.filter(this.calls(), function (call) {
        var isMatch = true;

        // match functions return true if filter is null
        isMatch = isMatch && call.isFieldMatch('queue', this.callQueueFilter());
        isMatch = isMatch && call.isFieldMatch('media', this.callMediaFilter());
        isMatch = isMatch && call.isFieldMatch('line', this.callLineFilter());
        isMatch = isMatch && call.isFieldMatch('customerName', this.callCustomerFilter());
        isMatch = isMatch && call.isFieldMatch('skillsReq', this.callSkillFilter());
        isMatch = isMatch && call.isFieldMatch('node', this.callNodeFilter());

        return isMatch;
      }, this);
    }, this);

    this.customerList = ko.computed(function () {
      return _.map(oa.clients(), function (client) {
        return client.name;
      }).sort();
    });

    this.lineList = ko.computed(function () {
      return _.map(oa.lines(), function (line) {
        return line.name;
      }).sort();
    });

    this.skillList = oa.basicSkills;

    this.nodeList = ko.computed(function () {
      return oa.nodes().sort();
    });

    this.queueList = ko.computed(function () {
      return _.map(this.queues(), function (q) {
        return q.name;
      }).sort();
    }, this);

    this.callsSortIsAscendingMap = {
      'customerName': ko.observable(true),
      'line': ko.observable(true),
      'skillsReq': ko.observable(true),
      'agMatch': ko.observable(true),
      'tInQueue': ko.observable(true),
      'expecWait': ko.observable(true)
    };
    this.callsSortIconTitle = function (key) {
      return ko.computed(function () {
        return this.callsSortIsAscendingMap[key]() ? 'sort ascending' : 'sort descending';
      }, this);
    };

    this.callsSortKey = ko.observable('tInQueue');
    this.areCallsSortedBy = function (key) {
      return ko.computed(function () {
        return this.callsSortKey() === key;
      }, this);
    };

    this.sortCallsBy = function (key, keepOrder) {
      if (!key) {
        key = this.callsSortKey();
      }

      var isAscending = this.callsSortIsAscendingMap[key]();

      if (keepOrder) {
        isAscending = !isAscending; // use previous sort order
      }

      this.calls.sort(function (l, r) {
        var left = l[key].call(l);
        var right = r[key].call(l);
        if (isAscending) {
          return left === right ? 0 : (left < right ? -1 : 1);
        }
        else {
          return right === left ? 0 : (right < left ? -1 : 1);
        }
      });

      this.callsSortKey(key);
      this.callsSortIsAscendingMap[key](!isAscending);
    };
    // end Calls section

    this.initialize = function () {
      this.fetchInit();

      amplify.subscribe(oa.events.RECEIVEDQUEUECOUNT, this, function (d) {
        var pushList = [];
        var removeList = [];

        _.each(d.data, function (val, name) {
          var entry = _.find(this.queues(), function (q) {
            return q.name === name;
          });

          if (_.isObject(val)) {
            if (_.isObject(entry)) {
              entry.update(val);
            }
            else {
              var match = _.contains(oa.permissionsQueues(), name);
              if(match === true){
                var newEntry = new this.QueueInfo({
                  name: name
                }, this.qRollingStatsCoverage);
                newEntry.update(val);
                pushList.push(newEntry);
              }
            }
          }
          else if (val === 'deleted') {
            removeList.push(entry);
          }
        }, this);


        if (removeList.length > 0) {
          this.queues.removeAll(removeList);
        }

        if (pushList.length > 0) {
          this.queues.push.apply(this.queues, pushList);
        }
      });

      amplify.subscribe(oa.events.UPDATEDQUEUEDCALLS, this, function (d) {
        var pushList = [];
        var removeList = [];

        _.each(d.data, function (n, u) {
          var entry = _.find(this.calls(), function (a) {
            return a.id === u;
          });

          if (_.isObject(n)) {
            if (_.isObject(entry)) {
              entry.update(n);
            }
            else {
              var match = _.contains(oa.permissionsQueues(), n.queue);
              if(match === true){
                n['id'] = u;
                pushList.push(new this.Call(n));
              }
            }
          }
          else if (n === 'deleted') {
            removeList.push(entry);
          }
        }, this);

        if (removeList.length > 0) {
          this.calls.removeAll(removeList);
        }

        if (pushList.length > 0) {
          this.calls.push.apply(this.calls, pushList);
          this.sortCallsBy(null, true);
        }
      });

      amplify.subscribe(oa.events.UPDATEDQUEUERSTAT, this, function (d) {
        _.each(d.data.updates, function (val) {
          var entry = _.find(this.queues(), function (q) {
            return q.name === val.queue;
          });

          if (_.isObject(entry)) {
            entry.updateRStat(val);
          }
        }, this);
      });
    };

    this.reset = function () {
      this.queues([]); // get_queues
      this.qRollingStatsCoverage('last_hr');
      this.queueMediaFilter('voice');

      this.calls([]);
      this.callQueueFilter();
      this.callMediaFilter(['voice', 'voicemail']);
      this.callLineFilter();
      this.callCustomerFilter();
      this.callSkillFilter();
      this.callNodeFilter();

      this.callsSortIsAscendingMap.customerName(true);
      this.callsSortIsAscendingMap.line(true);
      this.callsSortIsAscendingMap.skillsReq(true);
      this.callsSortIsAscendingMap.agMatch(true);
      this.callsSortIsAscendingMap.tInQueue(true);
      this.callsSortIsAscendingMap.expecWait(true);

      this.callsSortKey('tInQueue');

      this.fetchInit();
    };

    this.fetchInit = function () {

      oa.subscribeQueues(function (result) {

        var queues = _.filter(result, function (x) {
          var match = _.contains(oa.permissionsQueues() ,x.name);
          if(match === true){
            return x;
          }
        }, this);

        queues = _.map(queues, function (x) {
            return new this.QueueInfo(x, this.qRollingStatsCoverage);
        }, this);
        this.queues(queues);
      }, this);

      oa.subscribeQueuedCalls(function (result) {
        var calls = _.filter(result, function (x) {
          var match = _.contains(oa.permissionsQueues() ,x.queue);
          if(match === true){
            return x;
          }
        }, this);

        calls = _.map(calls, function (x) {
          return new this.Call(x);
        }, this);

        this.calls(calls);

        // unified timer for calls
        var self = this;
        if (this.intervalId) {
          clearInterval(this.intervalId);
        }
        this.intervalId = setInterval(function () {
          _.each(self.calls(), function (call) {
            call.tickQueuedSeconds();
          });
        }, 1000);
      }, this);

      oa.getClients();
      oa.getSkills();
      oa.getNodes();
      oa.getLines();
    };
  };

  // re-usable bindings
  var queueMediaFilterBinding = function (key, vm) {
    return {
      css: {
        'active': vm.queueMediaFilter() === key
      },
      event: {
        click: function () {
          vm.queueMediaFilter(key);
        }
      }
    };
  };
  var callMediaFilterBinding = function (key, vm) {
    return {
      css: {
        'active': _.contains(vm.callMediaFilter(), key)
      },
      event: {
        click: function () {
          if (_.contains(vm.callMediaFilter(), key)) {
            vm.callMediaFilter.remove(key);
          }
          else {
            vm.callMediaFilter.push(key);
          }
        }
      }
    };
  };
  var callsSortByBinding = function (key, vm) {
    return {
      attr: {
        title: vm.callsSortIconTitle(key)
      },
      css: {
        'active': vm.areCallsSortedBy(key)
      },
      event: {
        click: function () {
          vm.sortCallsBy(key);
        }
      }
    };
  };
  var callFilterBgBinding = function (valueBind) {
    if (!ko.isObservable(valueBind)) return {};

    return {
      css: {
        'active': Boolean(valueBind())
      }
    };
  };

  var bindings = {
    qmQueueStatsCoverage: function () {
      return {
        options: this.coverageList,
        optionsText: 'name',
        optionsValue: 'apiText',
        value: this.qRollingStatsCoverage
      };
    },
    qmQueues: function () {
      return {
        foreach: this.filteredQueues
      };
    },
    qmQueueName: function () {
      return {
        text: this.name
      };
    },
    qmQueueShowLinesIcon: function () {
      return {
        event: {
          click: function () {
            this.isExpanded(!this.isExpanded());
          }
        },
        css : {
          'arrow-down': this.isExpanded,
          'arrow-right': !this.isExpanded()
        }
      };
    },
    qmQueueLinesContainer: function () {
      return {
        visible: this.isExpanded
      };
    },
    qmQueueCiq: function (ctx) {
      var vm = ctx.$root;
      var q = this.name;
      return {
        // flashingText: this.ciq
        // work around to be safe for demo
        flashingText: _.filter(vm.calls(), function (call) {
          return call.isFieldMatch('queue', q);
        }).length
      };
    },
    qmQueueLines: function () {
      return {
        foreach: this.lines
      };
    },
    qmQueueLineExt: function () {
      return {
        text: this.ext
      };
    },
    qmQueueLineDidN: function () {
      return {
        text: this.didN,
        textNone: true
      };
    },
    qmQueueLineCustomer: function () {
      return {
        text: this.customerName
      };
    },
    qmQueueExpectedWait: function () {
      return {
        text: this.expectedWait
      };
    },
    qmQueueASpeedAnswer: function () {
      return {
        flashingText: this.rstatSelected().aSpeedAnswer
      };
    },
    qmQueueLongestWait: function () {
      return {
        flashingText: this.rstatSelected().longestWait
      };
    },
    qmQueueConnected: function () {
      return {
        flashingText: this.connected
      };
    },
    qmQueueCompleted: function () {
      return {
        flashingText: this.rstatSelected().completed
      };
    },
    qmQueueAAbandonTime: function () {
      return {
        flashingText: this.rstatSelected().aAbandonTime
      };
    },
    qmQueueAbandoned: function () {
      return {
        flashingText: this.rstatSelected().abandoned
      };
    },
    qmQueueMediaFilterVoice: function (ctx) {
      return queueMediaFilterBinding('voice', ctx.$data);
    },
    qmQueueMediaFilterEmail: function (ctx) {
      return queueMediaFilterBinding('email', ctx.$data);
    },
    qmQueueMediaFilterVoicemail: function (ctx) {
      return queueMediaFilterBinding('voicemail', ctx.$data);
    },
    qmCalls: function () {
      return {
        foreach: this.filteredCalls
      };
    },
    qmCallRow: function () {
      return {
        attr: {
          id: this.id
        },
        css: {
          'bg-lightgray': this.isExpanded,
          'bg-lightblue': !this.isExpanded()
        }
      };
    },
    qmCallShowTransferIcon: function () {
      return {
        event: {
          click: function () {
            this.isExpanded(!this.isExpanded());
          }
        },
        css : {
          'icon-arrow-down': this.isExpanded,
          'icon-caret-right': !this.isExpanded()
        }
      };
    },
    qmCallTransferRow: function () {
      return {
        visible: this.isExpanded
      };
    },
    qmCallMedia: function () {
      return {
        css: {
          'icon-phone': this.isMediaVoice,
          'icon-voicemail': this.isMediaVoicemail,
          'icon-email': this.isMediaEmail
        }
      };
    },
    qmCallCustomerAvatar: function () {
      return {
        attr: {
          src: this.customerAvatar
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
    qmCallCustomerName: function () {
      return {
        text: this.customerName
      };
    },
    qmCallCustomerCallerId: function () {
      return {
        text: this.customerCallerId
      };
    },
    qmCallLine: function () {
      return {
        text: this.line
      };
    },
    qmCallSkillsReq: function () {
      return {
        text: this.skillsReq
      };
    },
    qmCallAgMatch: function () {
      return {
        text: this.agMatch
      };
    },
    qmCallTInQueue: function () {
      return {
        text: this.tInQueue
      };
    },
    qmCallExpecWait: function () {
      return {
        text: this.expecWait
      };
    },
    qmCallsSortByCustomer: function (ctx) {
      return callsSortByBinding('customerName', ctx.$root);
    },
    qmCallsSortByLine: function (ctx) {
      return callsSortByBinding('line', ctx.$root);
    },
    qmCallsSortBySkillsReq: function (ctx) {
      return callsSortByBinding('skillsReq', ctx.$root);
    },
    qmCallsSortByAgMatch: function (ctx) {
      return callsSortByBinding('agMatch', ctx.$root);
    },
    qmCallsSortByTInQueue: function (ctx) {
      return callsSortByBinding('tInQueue', ctx.$root);
    },
    qmCallsSortByExpecWait: function (ctx) {
      return callsSortByBinding('expecWait', ctx.$root);
    },
    qmCallMediaFilterVoice: function (ctx) {
      return callMediaFilterBinding('voice', ctx.$root);
    },
    qmCallMediaFilterEmail: function (ctx) {
      return callMediaFilterBinding('email', ctx.$root);
    },
    qmCallMediaFilterVoicemail: function (ctx) {
      return callMediaFilterBinding('voicemail', ctx.$root);
    },
    qmCallQueueFilter: function () {
      return {
        options: this.queueList,
        value: this.callQueueFilter,
        optionsCaption: 'Any Queue'
      };
    },
    qmCallLineFilter: function () {
      return {
        options: this.lineList,
        value: this.callLineFilter,
        optionsCaption: 'Any Line'
      };
    },
    qmCallCustomerFilter: function () {
      return {
        options: this.customerList,
        value: this.callCustomerFilter,
        optionsCaption: 'Any Customer'
      };
    },
    qmCallSkillFilter: function () {
      return {
        options: this.skillList,
        value: this.callSkillFilter,
        optionsCaption: 'Any Skill'
      };
    },
    qmCallNodeFilter: function () {
      return {
        options: this.nodeList,
        value: this.callNodeFilter,
        optionsCaption: 'Any Node'
      };
    },
    qmCallQueueFilterBg: function () {
      return callFilterBgBinding(this.callQueueFilter);
    },
    qmCallLineFilterBg: function () {
      return callFilterBgBinding(this.callLineFilter);
    },
    qmCallCustomerFilterBg: function () {
      return callFilterBgBinding(this.callCustomerFilter);
    },
    qmCallSkillFilterBg: function () {
      return callFilterBgBinding(this.callSkillFilter);
    },
    qmCallNodeFilterBg: function () {
      return callFilterBgBinding(this.callNodeFilter);
    }
  };

  ko.bindingProvider.instance.registerBindings(bindings);

  return {
    ViewModel: QueueManagerVM,
    Parent: Parent,
    meta: meta,
    template: _.template(template, {})
  };
});
