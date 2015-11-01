define([
  'knockout'
], function (ko) {
  var HeaderVM = _.once(function () {
    var self = this;

    this.date = ko.observable(new Date());

    this.dateStr = ko.computed(function () {
      var d = self.date();
      return d.format('mm / dd / yyyy');
    });

    this.timeStr = ko.computed(function () {
      var d = self.date();
      return d.format('shortTime');
    });

    this.welcomeName = ko.observable('');
    this.hasWelcomeName = ko.computed(function () {
      return Boolean(self.welcomeName());
    });
    this.logoutLink = $('#logout-link').attr('href');
  });
// console.log($('.date').getClassBindings(ko))

  var getNextMinuteSleepMillis = function () {
    var nowMillis = (new Date()).getTime();
    var curMinute = Math.floor(nowMillis / 60000);
    var nextMinuteMillis = (curMinute + 1) * 60000;
    var sleepMillis = nextMinuteMillis - nowMillis;

    return sleepMillis;
  };

  var updateHeaderDate = function () {
    headerVM.date(new Date());
  };

  var startTimePoll = function () {
    var sleepMillis = getNextMinuteSleepMillis();

    setTimeout(function () {
      updateHeaderDate();
      startTimePoll();
    }, sleepMillis);
  };

  var headerVM = new HeaderVM();
  var bindings = {
    'dateStr': { text: headerVM.dateStr },
    'timeStr': { text: headerVM.timeStr },
    'navbarUsername': {text: headerVM.welcomeName },
    'navbarGreeting': {visible: headerVM.hasWelcomeName},
    'logoutLink': {
      click: function () {
        $.oucUtils.clearSessionValues();
        return true;
      }
    }
  };

  ko.bindingProvider.instance.registerBindings(bindings);
  ko.applyBindings(headerVM, document.getElementById('header'));

  startTimePoll();

  return headerVM;
});