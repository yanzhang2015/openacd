require([
  'defaults'
], function () {
  // additional utils if DEBUG=true
  LESSCSS = false;

  oa = null;
  am = null;
  as = null;
  ms = null;
  oc = null;
  qm = null;
  sm = null;

  $.oucUtils.colorizeDashboard = function () {
    $(['<style type="text/css">',
       '.dashboard-container { background-color: #effefe; }',
       '#dcontent .gadget { background-color: #fefeef; }',
       '#dcontent ul.gridster-wrapper { background-color: #feefef; }',
       '#dcontent li.gridster-widget { background-color: #effeef; }',
       '</style>'].join('')).appendTo('head');
  };

  $.oucUtils.loadGlobals = function () {
    require([
      'models/openacd',
      'gadgets/agent-manager/scripts/agent-manager',
      'gadgets/agent-state/scripts/agent-state',
      'gadgets/call-recording/scripts/call-recording',
      'gadgets/my-statistics/scripts/my-statistics',
      'gadgets/outbound-call/scripts/outbound-call',
      'gadgets/queue-manager/scripts/queue-manager',
      'gadgets/session-manager/scripts/session-manager'
    ], function (oa1, am1, as1, cr1, ms1, oc1, qm1, sm1) {
      oa = oa1;
      am = am1;
      as = as1;
      cr = cr1;
      ms = ms1;
      oc = oc1;
      qm = qm1;
      sm = sm1;
      console.log('globals ready.');
    });

    return 'loading modules...';
  };

  var loadCss = $.oucUtils.loadCss;
  // override
  $.oucUtils.loadCss = function () {
    arguments[0] = arguments[0] + '?bust=' + (new Date()).getTime();
    return loadCss.apply(this, arguments);
  };

  if (LESSCSS) {
    (function () {
      // array of .less file locations
      // can only be called once (on page load)
      var links = _.each([
        'gadgets/agent-manager/styles/agent-manager.less',
        'gadgets/agent-state/styles/agent-state.less',
        'gadgets/call-recording/styles/call-recording.less',
        'gadgets/my-statistics/styles/my-statistics.less',
        'gadgets/outbound-call/styles/outbound-call.less',
        'gadgets/queue-manager/styles/queue-manager.less',
        'gadgets/session-manager/styles/session-manager.less'
      ], function (href) {
        $('<link />').attr('rel', 'stylesheet/less')
          .attr('href', href + '?bust=' + (new Date()).getTime())
          .appendTo('head');
      });

      require(['less']);
    })();
  }
});
