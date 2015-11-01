define(['knockout'], function (ko) {
  var NavVM = _.once(function () {
    this.$el = $('#dnav');
    this.tabs = this.$el.find('[data-toggle="tabgrp"] a').map(function (elem) {
      return $(this).text().trim().toLowerCase();
    });
    this.tabScrollPositions = [];
    this.appliedLocalSettings = false;
    this.isPinned = ko.observable(false);
    this.pushpinTitle = ko.computed(function () {
      if (this.isPinned()) {
        return 'Unpin tab bar';
      }
      else {
        return 'Pin tab bar';
      }
    }, this);
    this.isAddEnabled = ko.observable(false);
    this.current = -1;
    this.previous = this.current;
    this.validateCurrent = function () {
      if (this.current === -1) {
        if (this.previous === -1) {
          this.current = 0;
        }
        else {
          this.current = this.previous;
        }
      }
      $.bbq.pushState('#' + this.tabs[this.current]); // change the url if invalid
    };

    this.setCurrentTab = function (tabStr, e) {
      tabStr = tabStr ? tabStr : '#' + this.tabs[0];
      this.previous = this.current;
      this.current = _.indexOf(this.tabs, tabStr.replace('#', ''));
      if (this.previous === this.current) return;

      if (e === 'hashchange') {
        this.validateCurrent(); // user might enter invalid fragment
        this.$el.find('[data-toggle="tabgrp"]:eq(' + this.current + ')').tabgrp('show');
      }
      else {
        // no need to validate, cannot be user input
        $.bbq.pushState(tabStr);
        if (self.isPinned()) {
          var scrollPosition = self.tabScrollPositions[self.current];
          self.setScroll(scrollPosition.top, scrollPosition.left);
        }
      }

      this.setWidth();
    };

    this.getCurrent = function () {
      return this.current;
    };

    this.getCurrentTab = function () {
      return this.tabs[this.current];
    };

    this.setWidth = function () {
      var c = this.$el.find('[data-toggle="tabgrp"]:eq(' + this.current + ')'),
          a = c.nextAll('ul.dropdown-menu').find('a[data-class="tabOptSwitch"]'),
          l = a.length > 0 ? a.attr('href').replace('#', '') : '';

      // wide target means narrow layout
      if (l === 'wide') {
        this.$el.parent().removeClass('wide');
        this.$el.parent().addClass('narrow');
      }
      else if (l === 'narrow') {
        this.$el.parent().addClass('wide');
        this.$el.parent().removeClass('narrow');
      }
      else {
        this.$el.parent().removeClass('wide');
        this.$el.parent().removeClass('narrow');
      }
    };

    this.hideTab = function (str) {
      this.$el.find('a[data-target="#' + str + '-tab"]').parents('.tab-group').hide();
    };

    this.pin = function () {
      this.isPinned(true);
      this.$el.parent().addClass('pinned');
      this.storeSettings();
    };

    this.unpin = function () {
      this.resetScrollPositions();
      this.isPinned(false);
      this.$el.parent().removeClass('pinned');
      this.storeSettings();
    };

    this.saveScrollPosition = function () {
      this.tabScrollPositions[this.current].top = $(document).scrollTop();
      this.tabScrollPositions[this.current].left = $(document).scrollLeft();
    };

    this.resetScrollPositions = function () {
      this.tabScrollPositions = _.map(this.tabs, function () {
        return {
          top: 0,
          left: 0
        };
      });
    };

    this.setScroll = function (top, left) {
      $(document).scrollTop(top);
      $(document).scrollLeft(left);
    };

    this.storeSettings = function () {
      var obj = {
        isPinned: this.isPinned()
      };
      $.oucUtils.storeLocallyForUser('NAVBAR', obj);
    };

    this.applyLocalSettings = function () {
      if (this.appliedLocalSettings) return;

      var obj = $.oucUtils.getValueFromLocalForUser('NAVBAR');
      if (_.isObject(obj)) {
        if (obj.isPinned) {
          this.$el.parent().addClass('pinned');
          this.isPinned(true);
        }
      }
      this.appliedLocalSettings = true;
    };

    var self = this;
    this.$el.find('[data-toggle="tabgrp"] a').on('shown', function (e) {
      self.setCurrentTab($(e.target).attr('href'), 'shown');
    }).on('click', function () {
      if (self.isPinned()) self.saveScrollPosition();
    });
    this.resetScrollPositions();
  });

  // default callbacks
  NavVM.prototype.onClickSwitchToNarrow = function () {};
  NavVM.prototype.onClickSwitchToWide = function () {};
  NavVM.prototype.onClickAddWidget = function () {};

  var navVM = new NavVM();
  var bindings = {
    'tabOptSwitch': {
      click: function (ctx, e) {
        var href = $(e.target).attr('href').replace('#', '');
        if (href === 'narrow') {
          navVM.onClickSwitchToNarrow();
          navVM.$el.parent().removeClass('wide');
          $(e.target).attr('href', '#wide').text('Switch to Wide Layout');
        }
        else if (href === 'wide') {
          navVM.onClickSwitchToWide();
          navVM.$el.parent().addClass('wide');
          $(e.target).attr('href', '#narrow').text('Switch to Narrow Layout');
        }
      }
    },
    'tabOptAdd': {
      click: function () {
        navVM.onClickAddWidget();
      },
      enabled: navVM.isAddEnabled
    },
    'tabPushpin': {
      css: {
        'pinned': navVM.isPinned
      },
      attr: {
        'title': navVM.pushpinTitle
      },
      click: function () {
        if (navVM.isPinned()) {
          navVM.unpin();
        }
        else {
          navVM.pin();
        }
      }
    }
  };

  ko.bindingProvider.instance.registerBindings(bindings);
  ko.applyBindings(navVM, navVM.$el[0]);

  navVM.setCurrentTab($.param.fragment(), 'hashchange');

  $(window).on('hashchange', function (e) {
    navVM.setCurrentTab(e.fragment, 'hashchange');
  });

  return navVM;
});