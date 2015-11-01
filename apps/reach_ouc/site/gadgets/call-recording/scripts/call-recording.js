define([
  'knockout',
  'buzz',
  'models/timer',
  'models/gadget',
  'models/openacd',
  'json!../meta/call-recording.json',
  'text!../templates/call-recording.html'
], function (ko, buzz, Timer, Parent, oa, meta, template) {
  var CallRecordingVM = function () {
    this.Recording = function (info) {
      this.callId = info.call_id;
      this.url = info.recording_location;
      this.hasFile = info.file_exists;

      this.startTs = ko.observable(info.start_timestamp);
      this.callerId = ko.observable(info.caller_id);
      this.callerAni = ko.observable(info.caller_ani);
      this.skills = ko.observableArray(oa.filterBasicSkills(info.skills));
      this.customer = ko.observable(info.client);
      this.customerAvatar = ko.observable('');
      this.queue = ko.observable(info.queue);
      this.line = ko.observable(info.line);
      this.agent = ko.observable(info.agent);
      this.duration = ko.observable(Math.round(info.recording_ms / 1000));

      this.isExpanded = ko.observable(false);
      this.isPlayable = ko.observable(true);
      this.isLoading = ko.observable(false);
      this.playbackPosition = ko.observable(0);
      this.isPaused = ko.observable(true);
      this.isStopped = ko.observable(true);
      this.soundFileMessage = ko.observable('Waiting for audio file to load');

      this.skillsText = ko.computed(function () {
        return this.skills().join(', ');
      }, this);
      this.startDateText = ko.computed(function () {
        var d = new Date(this.startTs());
        return d.format('mm / dd / yyyy');
      }, this);
      this.startTimeText = ko.computed(function () {
        var d = new Date(this.startTs());
        return d.format('HH:MM:ss');
      }, this);
      this.durationText = ko.computed(function () {
        return Timer.prototype.utils.formatMMSS(this.duration());
      }, this);

      oa.getClient(info.client, function (obj) {
        this.customerAvatar(obj.avatar);
      }, this);

      if (!this.hasFile) {
        this.isPlayable(false);
        this.soundFileMessage('Audio file not available');
        this.sound = null;
      }
      else {
        this.sound = new buzz.sound(this.url, {
          preload: 'none'
        });

        var self = this;
        this.sound.bind('timeupdate', function (e) {
          self.isLoading(false);
          self.playbackPosition(this.getTime());
        });
        this.sound.bind('ended', function (e) {
          self.isLoading(false);
          self.playbackPosition(self.duration());
          self.playbackPosition(0);
          self.isStopped(true);
        });
        this.sound.bind('pause', function (e) {
          self.isPaused(true);
        });
        this.sound.bind('play playing', function (e) {
          self.isPaused(false);
          self.isStopped(false);
        });
        this.sound.bindOnce('loadstart', function (e) {
          self.isLoading(true);
        });
        this.sound.bind('loadeddata', function (e) {
          self.isLoading(false);
        });
        this.sound.bind('waiting', function (e) {
          self.setLoading();
        });
        this.sound.bind('error', function () {
          if (self.sound.getStateCode() === 0) {
            self.isPlayable(false);
            self.soundFileMessage('Audio file not available');
          }
        });

        var subscription = this.isExpanded.subscribe(function (val) {
          if (!val) return;

          if (this.isLoading()) {
            // firefox: loadstart event fires even if preload=none
            // GET Partial content only happens when play() is called
            this.sound.play().stop();
          }
          else {
            this.sound.load();
          }

          subscription.dispose(); // bind once
        }, this);
      }

      this.seek = function (seconds) {
        if (!this.hasFile) return;

        var current = this.sound.getTime();
        this.sound.setTime(current + seconds);
        this.setLoading();
      };

      this.onSlide = function (event, ui) {
        if (!this.hasFile) return;

        this.sound.setTime(ui.value);
        this.setLoading();
      };

      this.setLoading = function () {
        if (!this.isPaused() && !this.isStopped()) {
          this.isLoading(true);
        }
      };
    };

    this.recordings = ko.observableArray();
    this.page = ko.observable(1);
    this.recordingsCount = ko.observable(0);
    this.recordingsPerPage = 13;
    this.lastRecording = ko.computed(function () {
      return this.page() * this.recordingsPerPage;
    }, this);
    this.firstRecording = ko.computed(function () {
      return this.lastRecording() - this.recordingsPerPage + 1;
    }, this);

    this.callIdFilterBuffer = ko.observable('');
    this.callIdFilter = ko.computed(function () {
      return this.callIdFilterBuffer();
    }, this).extend({ throttle: 300 });
    this.startFilter = ko.observable();
    this.endFilter = ko.observable();
    this.queueFilter = ko.observable('');
    this.lineFilter = ko.observable('');
    this.customerFilter = ko.observable('');
    this.callerIdFilterBuffer = ko.observable('');
    this.callerIdFilter = ko.computed(function () {
      return this.callerIdFilterBuffer();
    }, this).extend({ throttle: 300 });
    this.agentFilterBuffer = ko.observable('');
    this.agentFilter = ko.computed(function () {
      return this.agentFilterBuffer();
    }, this).extend({ throttle: 300 });
    this.skillFilter = ko.observable('');
    this.filterOpts = ko.computed(function () {
      return {
        'call_id': this.callIdFilter(),
        'queue': this.queueFilter(),
        'line': this.lineFilter(),
        'client': this.customerFilter(),
        'caller_id': this.callerIdFilter(),
        'agent': this.agentFilter(),
        'skill': this.skillFilter(),
        'start_timestamp': {
          'from': this.startFilter(),
          'to': this.endFilter()
        }
      };
    }, this);

    this.queueList = ko.computed(function () {
      return _.map(oa.queues(), function (queue) {
        return queue.name;
      }).sort();
    });
    this.lineList = ko.computed(function () {
      return _.map(oa.lines(), function (line) {
        return line.name;
      }).sort();
    });
    this.customerList = ko.computed(function () {
      return _.map(oa.clients(), function (client) {
        return client.name;
      }).sort();
    });
    this.skillList = oa.basicSkills;

    this.sortIsAscendingMap = {
      'start_timestamp': ko.observable(true),
      'client': ko.observable(false),
      'queue': ko.observable(true),
      'line': ko.observable(true),
      'agent': ko.observable(true),
      'recording_ms': ko.observable(true)
    };
    this.sortedBy = ko.observable('client');
    this.sortOpts = ko.observable({
      'client': 1
    });

    this.sortBy = function (key) {
      var isAscending = this.sortIsAscendingMap[key]();
      this.sortedBy(key);

      var obj = {};
      var value = isAscending ? 1 : -1;
      obj[key] = value;
      this.sortOpts(obj);

      this.sortIsAscendingMap[key](!isAscending);
    };

    this.stopOthers = function (callId) {
      _.each(this.recordings(), function (rec) {
        if (rec.callId === callId) return;
        if (!rec.hasFile) return;
        rec.sound.stop().trigger('ended');
      });
    };

    this.stopAll = function () {
      buzz.all().stop().trigger('ended');
    };

    this.collapseAll = function () {
      this.stopAll();
      _.each(this.recordings(), function (rec) {
        rec.isExpanded(false);
      });
    };

    this.getRecordings = function () {
      if (this.endFilter() < this.startFilter()) {
        this.recordings([]);
        this.recordingsCount(0);
        return;
      }

      this.stopAll();
      buzz.sounds = [];
      oa.getRecordings(this.filterOpts(), this.sortOpts(), this.firstRecording(), this.lastRecording(), function (result) {
        var arr = _.map(result.recordings, function (rec) {
          return new this.Recording(rec);
        }, this);

        this.recordings(arr);
        this.recordingsCount(result.total_entries);
      }, this);
    };

    this.resetDateTimeFilters = function () {
      var d = new Date();

      var end = new Date(d);
      end.setHours(23);
      end.setMinutes(59);
      end.setSeconds(59);
      end.setMilliseconds(999);

      d.setDate(d.getDate() - 1); // yesterday

      var start = new Date(d);
      start.setHours(0);
      start.setMinutes(0);
      start.setSeconds(0);
      start.setMilliseconds(0);

      this.startFilter(start.getTime());
      this.endFilter(end.getTime());
    };

    this.reset = function () {
      this.stopAll();
      buzz.sounds = [];

      this.recordings([]);
      this.page(1);
      this.recordingsCount(0);
      this.callIdFilterBuffer('');
      this.queueFilter('');
      this.lineFilter('');
      this.customerFilter('');
      this.callerIdFilterBuffer('');
      this.agentFilterBuffer('');
      this.skillFilter('');
      this.sortIsAscendingMap['start_timestamp'](true);
      this.sortIsAscendingMap['client'](false);
      this.sortIsAscendingMap['queue'](true);
      this.sortIsAscendingMap['line'](true);
      this.sortIsAscendingMap['agent'](true);
      this.sortIsAscendingMap['recording_ms'](true);
      this.sortedBy('client');
      this.sortOpts({
        'client': 1
      });

      this.resetDateTimeFilters();
      this.fetchInit();
    };

    this.initialize = function () {
      this.getRecordingsBuffer = ko.computed(function () {
        this.filterOpts();
        this.sortOpts();
        this.firstRecording();
        this.lastRecording();

        // console.log(this.filterOpts(), this.sortOpts(), this.firstRecording(), this.lastRecording())

        return this.getRecordings();
      }, this).extend({ throttle: 500 });

      this.resetDateTimeFilters();
      this.fetchInit();
    };

    this.fetchInit = function () {
      oa.getClients();
      oa.getSkills();
      oa.getLines();
      oa.getQueues();
    };
  };


  var sortByBinding = function (key, vm) {
    return {
      attr: {
        title: vm.sortIsAscendingMap[key]() ? 'sort ascending' : 'sort descending'
      },
      css: {
        'active': vm.sortedBy() === key
      },
      event: {
        click: function () {
          vm.sortBy(key);
        }
      }
    };
  };
  var bindings = {
    crRecordings: function () {
      return {
        foreach: this.recordings
      };
    },
    crRecording: function () {
      return {
        css: {
          'expanded': this.isExpanded
        }
      };
    },
    crRecordingVisibleWhenPlayable: function () {
      return {
        visible: this.isPlayable
      };
    },
    crSoundFileMessage: function () {
      return {
        visible: !this.isPlayable(),
        text: this.soundFileMessage
      };
    },
    crRecordingArrow: function () {
      return {
        click: function () {
          var isExpanded = this.isExpanded();
          if (isExpanded) {
            this.isExpanded(false);
          }
          else {
            this.isExpanded(true);
          }
        }
      };
    },
    crRecordingSlider: function (ctx) {
      return {
        'slider': this.playbackPosition,
        'sliderOptions': {
          orientation: 'horizontal',
          range: 'min',
          min: 0,
          max: this.duration,
          stop: this.onSlide.bind(ctx.$data)
        }
      };
    },
    crRecordingBackToStart: function () {
      return {
        click: function () {
          if (!this.hasFile) return;

          var isPaused = this.sound.isPaused();
          this.sound.stop().trigger('ended');
          if (!isPaused) this.sound.play();
        }
      };
    },
    crRecordingRewind10: function () {
      return {
        click: function () {
          this.seek(-10);
        }
      };
    },
    crRecordingRewind2: function () {
      return {
        click: function () {
          this.seek(-2);
        }
      };
    },
    crRecordingPlayback: function (ctx) {
      return {
        click: function () {
          if (!this.hasFile) return;

          ctx.$root.stopOthers(ctx.$data.callId);
          this.sound.togglePlay();
        },
        css: {
          'play': this.isPaused() || this.isStopped(),
          'pause': !this.isPaused(),
          'loader': this.isLoading
        }
      };
    },
    crRecordingForward2: function () {
      return {
        click: function () {
          this.seek(2);
        }
      };
    },
    crRecordingForward10: function () {
      return {
        click: function () {
          this.seek(10);
        }
      };
    },
    crRecordingDownload: function () {
      return {
        click: function () {
          window.open(this.url);
        }
      };
    },
    crRecordingStartDate: function () {
      return {
        text: this.startDateText
      };
    },
    crRecordingStartTime: function () {
      return {
        text: this.startTimeText
      };
    },
    crRecordingCustomerAvatar: function () {
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
    crRecordingCustomerName: function () {
      return {
        text: this.customer,
        'titleOnTruncate': true
      };
    },
    crRecordingQueue: function () {
      return {
        text: this.queue,
        'titleOnTruncate': true
      };
    },
    crRecordingLine: function () {
      return {
        text: this.line,
        'titleOnTruncate': true
      };
    },
    crRecordingAgent: function () {
      return {
        text: this.agent,
        'titleOnTruncate': true
      };
    },
    crRecordingDuration: function () {
      return {
        text: this.durationText
      };
    },
    crRecordingCallId: function () {
      return {
        text: this.callId
      };
    },
    crRecordingCallerId: function () {
      return {
        text: this.callerId
      };
    },
    crRecordingCallerAni: function () {
      return {
        text: this.callerAni
      };
    },
    crRecordingSkills: function () {
      return {
        text: this.skillsText
      };
    },
    crCallIdFilter: function () {
      return {
        value: this.callIdFilterBuffer,
        valueUpdate: 'keyup'
      };
    },
    crStartFilter: function () {
      return {
        'datepicker': this.startFilter
      };
    },
    crEndFilter: function () {
      return {
        'datepicker': this.endFilter,
        'datepickerOptions': {
          'endOfDay': true
        }
      };
    },
    crQueueFilter: function () {
      return {
        options: this.queueList,
        value: this.queueFilter,
        optionsCaption: 'Any Queue'
      };
    },
    crLineFilter: function () {
      return {
        options: this.lineList,
        value: this.lineFilter,
        optionsCaption: 'Any Line'
      };
    },
    crCustomerFilter: function () {
      return {
        options: this.customerList,
        value: this.customerFilter,
        optionsCaption: 'Any Customer'
      };
    },
    crCallerIdFilter: function () {
      return {
        value: this.callerIdFilterBuffer,
        valueUpdate: 'keyup'
      };
    },
    crAgentFilter: function () {
      return {
        value: this.agentFilterBuffer,
        valueUpdate: 'keyup'
      };
    },
    crSkillFilter: function () {
      return {
        options: this.skillList,
        value: this.skillFilter,
        optionsCaption: 'Any Skill'
      };
    },
    crCallIdFilterBg: function () {
      return {
        css: { 'active': !!this.callIdFilter() }
      };
    },
    crQueueFilterBg: function () {
      return {
        css: { 'active': !!this.queueFilter() }
      };
    },
    crLineFilterBg: function () {
      return {
        css: { 'active': !!this.lineFilter() }
      };
    },
    crCustomerFilterBg: function () {
      return {
        css: { 'active': !!this.customerFilter() }
      };
    },
    crSkillFilterBg: function () {
      return {
        css: { 'active': !!this.skillFilter() }
      };
    },
    crSortByStart: function (ctx) {
      return sortByBinding('start_timestamp', ctx.$root);
    },
    crSortByCustomer: function (ctx) {
      return sortByBinding('client', ctx.$root);
    },
    crSortByQueue: function (ctx) {
      return sortByBinding('queue', ctx.$root);
    },
    crSortByLine: function (ctx) {
      return sortByBinding('line', ctx.$root);
    },
    crSortByAgent: function (ctx) {
      return sortByBinding('agent', ctx.$root);
    },
    crSortByDuration: function (ctx) {
      return sortByBinding('recording_ms', ctx.$root);
    },
    crCollapseAll: function (ctx) {
      return {
        click: this.collapseAll
      };
    },
    crRefresh: function () {
      return {
        click: this.getRecordings.bind(this),
        css: {
          'nomatch': this.recordingsCount() < 1
        }
      };
    },
    crPagination: function () {
      return {
        visible: this.recordingsCount() > 0,
        'pagination': this.page,
        'paginationOptions': {
          items: this.recordingsCount,
          itemsOnPage: this.recordingsPerPage,
          nextText: 'next page'
        }
      };
    },
    crNoMatchMessage: function () {
      return {
        visible: this.recordingsCount() < 1
      };
    }
  };

  ko.bindingProvider.instance.registerBindings(bindings);

  return {
    ViewModel: CallRecordingVM,
    Parent: Parent,
    meta: meta,
    template: _.template(template, {})
  };
});

