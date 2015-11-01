define([
  'knockout'
], function (ko) {
  // countdown option requires countdown limit
  // display starts to count up when countdown is over
  var Timer = function (isCountdown, countdownSeconds) {
    this.isStarted = ko.observable(false);
    this.totalSeconds = ko.observable(0);

    this.isCountdown = ko.observable(Boolean(isCountdown));
    this.countdownSeconds = ko.observable(Number(countdownSeconds));
    this.isCountdownOver = ko.observable(false);

    this.currentSeconds = ko.computed(function () {
      var currentSeconds = this.totalSeconds();
      if (this.isCountdown()) {
        currentSeconds = this.countdownSeconds() - currentSeconds;
        if (currentSeconds < 0) {
          currentSeconds = this.totalSeconds() - this.countdownSeconds();
        }
      }

      return currentSeconds;
    }, this);

    this.toMMSS = ko.computed(function () {
      return this.utils.formatMMSS(this.currentSeconds());
    }, this);

    this.toHHMMSS = ko.computed(function () {
      return this.utils.formatHHMMSS(this.currentSeconds());
    }, this);

    this.toHHMM = ko.computed(function () {
      return this.utils.formatHHMM(this.currentSeconds());
    }, this);

    this.start = function (initialSeconds) {
      if (this.isStarted()) return;
      if (this.isCountdown() && !_.isFinite(this.countdownSeconds())) return;

      initialSeconds = Number(initialSeconds);
      if (_.isFinite(initialSeconds)) {
        this.totalSeconds(initialSeconds);
      }

      this.isStarted(true);
      var self = this;

      this.intervalId = setInterval(function () {
        var currentSeconds = self.totalSeconds() + 1;
        self.totalSeconds(currentSeconds);

        if (self.isCountdown() && ((self.countdownSeconds() - currentSeconds) <= 0)) {
          self.isCountdownOver(true);
        }
      }, 1000);
    };

    this.stop = function () {
      if (!this.isStarted()) return;
      this.isStarted(false);
      try {
        clearInterval(this.intervalId);
      } catch (e) {}
    };

    this.reset = function () {
      this.stop();
      this.totalSeconds(0);
      this.isCountdownOver(false);
    };
  };

  // biggest unit (hours or minutes) can be more than 2 digits
  Timer.prototype.utils = {
    getDisplay: function (n) {
      var display = '' + n;
      if (n < 10) { display = '0' + n; }
      if (n < 0) { display = '00'; }

      return display;
    },
    formatMMSS: function (seconds) {
      var s = Math.floor(seconds % 60);
      var m = Math.floor(seconds / 60);

      return this.getDisplay(m) + ':' + this.getDisplay(s);
    },
    formatHHMMSS: function (seconds) {
      var s = Math.floor(seconds % 60);
      var m = Math.floor((seconds / 60) % 60);
      var h = Math.floor((seconds / 60) / 60);

      return this.getDisplay(h) + ':' + this.getDisplay(m) + ':' + this.getDisplay(s);
    },
    formatHHMM: function (seconds) {
      var m = Math.floor((seconds / 60) % 60);
      var h = Math.floor((seconds / 60) / 60);

      return this.getDisplay(h) + ':' + this.getDisplay(m);
    }
  };

  return Timer;
});