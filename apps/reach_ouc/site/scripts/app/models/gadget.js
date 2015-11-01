define([
  'knockout'
], function (ko) {
  var Gadget = function (meta, vm, el) {
    if (!meta) throw 'No gadget details available';

    this.meta = meta;
    this.vm = vm;
    this.el = el;
    this.isInit = false;
  };

  Gadget.prototype.render = function (container, stylesheet) {
    // stylesheet optional, override stylesheet in meta
    if (stylesheet) {
      $.oucUtils.loadCss(stylesheet);
    }
    else {
      $.oucUtils.loadCss(this.meta.stylesheet);
    }

    this.$el = $(container).addClass(this.meta.container_class);
    this.$el.find('.gadget-content').html(this.el);
    this.$el.find('.gadget-header h3').text(this.meta.title);

    this.openHeight = this.$el.height();

    return this.$el;
  };

  Gadget.prototype.attachCallback = function (event, callback) {
    if (!$.isFunction(callback)) return;

    // attach to vm
    this.vm[event] = callback;
  };

  Gadget.prototype.initialize = function () {
    if (this.isInit) return 'Cannot re-initialize gadget';
    this.isInit = true;

    ko.applyBindings(this.vm, this.$el[0]);

    if ($.isFunction(this.vm.initialize)) {
      this.vm.initialize();
    }
  };

  Gadget.prototype.reset = function () {
    if ($.isFunction(this.vm.reset)) {
      this.vm.reset();
    }
  };

  // register bindings once: on module's initial load
  ko.bindingProvider.instance.registerBindings({
    'gadgetToggleArrow': {
      click: function (vm, e) {
        var ha = $(e.target).closest('.header-arrow');
        if (ha.length > 0) {
          ha.toggleClass('down');
          ha.toggleClass('right');
          var content = ha.parent().next('.gadget-content');
          if (content.is(':visible')) {
            vm.beforeSlideUp();
            content.slideUp();
          }
          else {
            vm.beforeSlideDown();
            content.slideDown();
          }
          return false;
        }
      }
    },
    'gadgetSectionHeader': {
      click: function (vm, e) {
        var ha = $(e.target).closest('.header-arrow');
        if (ha.length > 0) {
          ha.toggleClass('down');
          ha.toggleClass('right');
          var content = ha.parents('.header-h4').next('.section-content');
          if (content.is(':visible')) {
            vm.beforeSlideUp();
            content.slideUp();
          }
          else {
            vm.beforeSlideDown();
            content.slideDown();
          }
          return false;
        }
      }
    },
    viewModelValues: function (ctx) {
      return {
        value: ko.toJSON(ctx.$root, null, 2),
        text: ko.toJSON(ctx.$root, null, 2)
      };
    }
  });

  return Gadget;
});