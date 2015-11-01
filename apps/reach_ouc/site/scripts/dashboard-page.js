require(['./init'], function () {
  // require global libraries before initialization
  require([
    'domReady!',
    'dateFormat',
    'jquery',
    'jquery-ui',
    'underscore',
    'amplify',
    'bootstrap',
    'bootstrap-tabgrp',
    'bootbox',
    'bbq',
    'cookie',
    'resize',
    'gridster',
    'simplePagination',
    'defaults'
  ], function () {
    if ($.isFunction(bootbox.alert)) {
      window.alert = bootbox.alert;
    }

    if ($.isFunction(bootbox.confirm)) {
      window.confirm = bootbox.confirm;
    }

    if ($.isFunction(bootbox.prompt)) {
      window.prompt = bootbox.prompt;
    }

    if ($.isFunction($.oucUtils.notify)) {
      window.notify = $.oucUtils.notify;
    }

    require(['dashboard']);

    if (DEBUG) require(['../debug']);
  });
});
