define([
  'knockout'
], function (ko) {
  var WidgetAddVM = _.once(function () {
    this.$el = $('#add-widget-modal');
    this.availableWidgets = ko.observableArray();
    this.selectedWidget = ko.observable();
    this.selectedGrid = null;
    this.widetAddHandler = function (id) { console.log('no handler function: ' + id); };

    this.sortOptions = function () {
      this.availableWidgets.sort(function (l, r) {
        return l.title === r.title ? 0 : (l.title < r.title ? -1 : 1);
      });
    };
  });

  var widgetAddVM = new WidgetAddVM();
  var bindings = {
    'widgetSelect': {
      options: widgetAddVM.availableWidgets,
      optionsText: 'title',
      optionsValue: 'id',
      value: widgetAddVM.selectedWidget
    },
    widgetAddButton: function (ctx) {
      return {
        click: function () {
          var meta = _.find(ctx.$root.availableWidgets(), function (w) {
            return w.id === this.selectedWidget();
          }, ctx.$root);
          ctx.$root.widetAddHandler(meta);
          $(ctx.$root.$el).modal('hide');
          ctx.$root.selectedWidget('');
          return false;
        }
      };
    },
    widgetCancelButton: function (ctx) {
      return {
        click: function () {
          $(ctx.$root.$el).modal('hide');
          ctx.$root.selectedWidget('');
          return false;
        }
      };
    }
  };

  ko.bindingProvider.instance.registerBindings(bindings);
  ko.applyBindings(widgetAddVM, widgetAddVM.$el[0]);

  return widgetAddVM;
});