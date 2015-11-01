var requirejs, DEBUG;
requirejs.config({
  baseUrl: document.getElementById('main-script').getAttribute('data-main').replace(/\/[^\/]+$/, '/app'),

  paths: {
    'amplify': '../lib/amplify/amplify',
    'bootstrap': '../lib/bootstrap/bootstrap',
    'bootstrap-tabgrp': '../lib/bootstrap/plugins/bootstrap-tabgrp',
    'bootbox': '../lib/bootstrap/plugins/bootbox',
    'buzz': '../lib/buzz/buzz',
    'dateFormat': '../lib/core/date.format',
    'sprintf': '../lib/core/sprintf',
    'jquery': '../lib/jquery/jquery',
    'jquery-migrate': '../lib/jquery/jquery-migrate',
    'jquery-ui': '../lib/jquery/jquery-ui',
    'bbq': '../lib/jquery/plugins/jquery.ba-bbq',
    'resize': '../lib/jquery/plugins/jquery.ba-resize',
    'cookie': '../lib/jquery/plugins/jquery.cookie',
    'gridster': '../lib/jquery/plugins/jquery.gridster',
    'simplePagination': '../lib/jquery/plugins/jquery.simplePagination',
    'knockout': '../lib/knockout/knockout',
    'ko-mapping': '../lib/knockout/plugins/mapping',
    'ko-classBindingProvider': '../lib/knockout/plugins/classBindingProvider',
    'less': '../lib/less/less.min',
    'domReady': '../lib/require/plugins/domReady',
    'text': '../lib/require/plugins/text',
    'json': '../lib/require/plugins/json',
    'strophe': '../lib/strophe/strophe',
    'underscore': '../lib/underscore/underscore',
    'defaults': '../defaults',
    'templates': '../../templates',
    'gadgets': '../../gadgets'
  },

  urlArgs: DEBUG ? 'bust=' + (new Date()).getTime() : null,

  waitSeconds: 60, // DEBUG ? 5 : 15,

  shim: {
    'jquery-migrate': ['jquery'],

    'jquery-ui': ['jquery', 'jquery-migrate'],

    'bootstrap': ['jquery'],

    'bootbox': ['jquery', 'bootstrap'],

    'bootstrap-tabgrp': ['jquery', 'bootstrap'],

    'bbq': ['jquery', 'jquery-migrate'],

    'resize': ['jquery'],

    'amplify': ['jquery'],

    'cookie': ['jquery'],

    'gridster': ['jquery'],

    'simplePagination': ['jquery'],

    'ko-mapping': ['knockout'],

    'ko-classBindingProvider': ['knockout'],

    'defaults': ['jquery', 'jquery-migrate', 'jquery-ui', 'underscore']
  }
});

// wrap console.log
if (!DEBUG) {
  var consoleLog = console.log;
  console.log = function () {
    if (DEBUG) {
      return consoleLog.apply(this, arguments);
    }

    return !DEBUG;
  };
}
