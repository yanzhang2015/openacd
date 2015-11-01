define([
  'config',
  'knockout'
], function (config, ko) {
  var ReportsVM = _.once(function () {
    this.el = document.getElementById('reports-ui');
    this.$el = $(this.el);

    this.types = [{
      name: 'Agent Group Productivity',
      value: 'agent_group_productivity'
    },
    {
      name: 'Agent Productivity',
      value: 'agent_productivity'
    },
    {
      name: 'CDR',
      value: 'cdr'
    },
    {
      name: 'Queue Group Traffic Overview',
      value: 'queue_group_traffic'
    },
    {
      name: 'Queue Traffic Overview',
      value: 'queue_traffic'
    }];
    this.directions = [{
      name: 'Inbound',
      value: 'inbound'
    },
    {
      name: 'Outbound',
      value: 'outbound'
    }];
    this.root = config.reportsReachUrl + '/';
    this.source = ko.observable('');
    this.selectValue = ko.observable(null);
    this.query = ko.observable({});

    var now = new Date();
    now.setDate(now.getDate() - 1);
    var dateYday = now.format('mm/dd/yyyy');
    this.queryStartDate = ko.observable(dateYday);
    this.queryEndDate = ko.observable(dateYday);
    this.queryDirection = ko.observable(null);

    this.getSource = function (ext) {
      var source = this.source();
      if (!source) return;

      var s = this.root + this.source() + '.' + ext;

      if (this.queryString() !== '') {
        s = s + '?' + this.queryString();
      }

      return s;
    };
    this.renewCookie = function (callback, context, isSynchronous) {
      var result = false;
      $.ajax({
        type: 'POST',
        url: config.reportsAuthUrl,
        async: !isSynchronous,
        timeout: 30000,
        success: function (data, textStatus, jqXHR) {
          callback.call(context);
          if (isSynchronous) result = true;
        },
        error: function (jqXHR, textStatus, errorThrown) {
          console.log(textStatus, errorThrown);
          notify('Something went wrong in generating the report. Please try again.', 'error');
          if (isSynchronous) result = false;
        }
      });

      if (isSynchronous) return result;
    };
    this.generate = function () {
      var selectValue = this.selectValue();
      if (!selectValue) return;

      this.renewCookie(function () {
        var startTs = new Date(this.queryStartDate()).getTime();
        var endTs = new Date(this.queryEndDate()).setHours(23, 59, 59, 999);
        var obj = {
		  'ignorePagination': true,
          'START_DATE': startTs,
          'END_DATE': endTs
        };

        if (selectValue === 'cdr') {
          var direction = this.queryDirection();
          obj['DIRECTION'] = direction ? direction : '';
          this.queryDirection(direction);
        }

        this.query(obj);
        this.source(selectValue);
      }, this);
    };

    this.queryString = ko.computed(function () {
      var arr = [];
      _.each(this.query(), function (val, key) {
        arr.push(encodeURIComponent(key) + '=' + encodeURIComponent(val));
      });
      return arr.join('&');
    }, this);
    this.html = ko.computed(function () {
      return this.getSource('html');
    }, this);
    this.pdf = ko.computed(function () {
      return this.getSource('pdf');
    }, this);
    this.csv = ko.computed(function () {
      return this.getSource('csv');
    }, this);
    this.xls = ko.computed(function () {
      return this.getSource('xls');
    }, this);

    this.$el.find('.datepicker').datepicker({
      numberOfMonths: 2,
      showOtherMonths: true,
      changeMonth: true,
      changeYear: true
    });

    var iframe = this.$el.find('iframe');
    function resize() {
      var height = $(window).height() - 200;
      iframe.height(height);
    }

    $(function () {
      resize();
      $(window).resize(function () {
        resize();
      });
    });
  });

  var reportsVM = new ReportsVM();
  var bindings = {
    reportsPreview: {
      attr: {
        'src': reportsVM.html
      }
    },
    reportsPdf: {
      click: function () {
        if (!reportsVM.pdf()) return;
        return reportsVM.renewCookie(function () {}, reportsVM, true);
      },
      attr: {
        'href': reportsVM.pdf
      }
    },
    reportsCsv: {
      click: function () {
        if (!reportsVM.csv()) return;
        return reportsVM.renewCookie(function () {}, reportsVM, true);
      },
      attr: {
        'href': reportsVM.csv
      }
    },
    reportsXls: {
      click: function () {
        if (!reportsVM.xls()) return;
        return reportsVM.renewCookie(function () {}, reportsVM, true);
      },
      attr: {
        'href': reportsVM.xls
      }
    },
    reportsSelect: {
      options: reportsVM.types,
      optionsText: 'name',
      optionsValue: 'value',
      optionsCaption: 'Select report type',
      value: reportsVM.selectValue
    },
    reportsGenerate: {
      click: reportsVM.generate
    },
    reportsQueryStartDate: {
      value: reportsVM.queryStartDate
    },
    reportsQueryEndDate: {
      value: reportsVM.queryEndDate
    },
    reportsVisibleIfCdr: {
      visible: ko.computed(function () {
        return reportsVM.selectValue() === 'cdr';
      })
    },
    reportsDirectionSelect: {
      options: reportsVM.directions,
      optionsText: 'name',
      optionsValue: 'value',
      optionsCaption: 'Any direction',
      value: reportsVM.queryDirection
    }
  };

  ko.bindingProvider.instance.registerBindings(bindings);
  ko.applyBindings(reportsVM, reportsVM.el);

  return reportsVM;
});
