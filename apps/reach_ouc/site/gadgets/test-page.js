var requirejs, DEBUG = true, LESSCSS = true;
requirejs.config({
  baseUrl: document.getElementById('main-script').getAttribute('data-main').replace('gadgets/test-page', 'scripts/app'),

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

  waitSeconds: DEBUG ? 5 : 15,

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

require([
  'knockout',
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
  'resize',
  'cookie',
  'gridster',
  'simplePagination',
  'defaults'
], function (ko) {
  var gadgetName = window.location.hash.replace('#', '');

  // default to agent-manager
  if (!gadgetName) {
    window.location.hash = '#agent-manager';
    window.location.reload();
  }

  var id = gadgetName + '-gadget';
  var title = gadgetName.replace('-', ' ');

  $('title').text(title);
  $('.gadget').attr('data-gadgetid', id);
  $('a[href="#' + gadgetName + '"]').parent().addClass('active');

  // fetch gadget module
  require(['gadgets/' + gadgetName + '/scripts/' + gadgetName], function (module) {
    function G() {
      module.Parent.call(this, module.meta, (new module.ViewModel()), module.template);
    }
    G.prototype = module.Parent.prototype;

    var g = new G();

    g.render($('.gadget'));

    var href = gadgetName + '/styles/' + gadgetName;
    if (LESSCSS) {
      var lessHref = href + '.less?bust=' + (new Date()).getTime();
      $('<link />').attr('rel', 'stylesheet/less').attr('href', lessHref).appendTo('head');
      require(['less']);
    }
    else {
      var cssHref = href + '.css?bust=' + (new Date()).getTime();
      $('<link />').attr('type', 'text/css').attr('rel', 'stylesheet').attr('href', cssHref).appendTo('head');
    }

    require(['json!gadgets/' + gadgetName + '/meta/test-data.json'], function (data) {
      var vm = ko.mapping.fromJS(data);
      vm.initialize = function () {};
      ko.utils.extend(g.vm, vm);
      g.initialize();
      ko.applyBindings(g.vm, $('#view-model-json')[0]);
    });

    $(window).on('hashchange', function (e) {
      window.location.reload();
    });

    $('#view-model-json textarea').change(function () {
      var data = JSON.parse($(this).val());
      ko.mapping.fromJS(data, g.vm);
    });
  });
});
