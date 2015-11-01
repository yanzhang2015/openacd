require(['./init'], function () {
  // require global libraries before initialization
  require([
    'domReady!',
    'dateFormat',
    'jquery',
    'underscore',
    'defaults'
  ], function () {
    if ($.isFunction($.oucUtils.notify)) {
      window.notify = $.oucUtils.notify;
    }

    require(['login']);
  });
});