define(function (argument) {
  var baseUrl = '';
  var oaUrl = baseUrl + '/api/openacd';
  var imPrebindUrl = baseUrl + '/im/prebind';
  var imBindUrl = baseUrl + '/im/bind';
  var profileUrl = baseUrl + '/api/profile';
  var staticUrl = document.getElementById('main-script').getAttribute('data-main').replace(/scripts\/[^\/]+$/, '');

  var wsUrl = 'wss://' + window.location.hostname + ':8936/wsock';
  // insecure ws if http
  if (window.location.protocol === 'http:') {
    wsUrl = 'ws://' + window.location.hostname + ':8937/wsock';
  }

  var reportsUrl = 'https://' + window.location.hostname + '/jasperserver';
  var reportsReachUrl = reportsUrl + '/rest_v2/reports/reach';
  var reportsAuthUrl = window.location.href.replace(/\/dashboard.*/, '') + '/reports/auth';

  var gadgets = {
    'agent-state-gadget': {
      id: 1,
      isActive: true,
      module: 'gadgets/agent-state/scripts/agent-state',
      name: 'AgentState',
      supervisorTab: false,
      default: true
    },
    'session-manager-gadget': {
      id: 2,
      isActive: true,
      module: 'gadgets/session-manager/scripts/session-manager',
      name: 'SessionManager',
      supervisorTab: false,
      default: true
    },
    'my-statistics-gadget': {
      id: 3,
      isActive: true,
      module: 'gadgets/my-statistics/scripts/my-statistics',
      name: 'MyStatistics',
      supervisorTab: false,
      default: false
    },
    'outbound-call-gadget': {
      id: 4,
      isActive: true,
      module: 'gadgets/outbound-call/scripts/outbound-call',
      name: 'OutboundCall',
      supervisorTab: false,
      default: false
    },
    'agent-manager-gadget': {
      id: 5,
      isActive: true,
      module: 'gadgets/agent-manager/scripts/agent-manager',
      name: 'AgentManager',
      supervisorTab: true,
      default: false
    },
    'queue-manager-gadget': {
      id: 6,
      isActive: true,
      module: 'gadgets/queue-manager/scripts/queue-manager',
      name: 'QueueManager',
      supervisorTab: true,
      default: false
    },
    'call-recording-gadget': {
      id: 7,
      isActive: true,
      module: 'gadgets/call-recording/scripts/call-recording',
      name: 'CallRecording',
      supervisorTab: true,
      default: false
    }
  };

  var activeFeatures = {
    reports: true
  };

  var pingInterval = 15000,
      pongWait = 15000,
      sleepDuration = 180000,
      gridUnitX = 260,
      gridUnitY = 10,
      gridMarginX = 10,
      gridMarginY = 10;

  var logOptions = {
    ALL: 'all',
    SUBSTR_FILTER: 'filter',
    SUBSTR_REJECT: 'reject',
    substrings: {
      filter: [],
      reject: []
    },
    generateConf: function (level) {
      if (level === this.ALL) {
        return new RegExp('.*');
      }
      else if (level === this.SUBSTR_FILTER) {
        return new RegExp(this.substrings.filter.join('|'), 'i');
      }
      else if (level === this.SUBSTR_REJECT) {
        return new RegExp('^((?!(' + this.substrings.reject.join('|') + ')).)*$', 'i');
      }
    }
  };

  logOptions.substrings.filter.push('result');
  logOptions.substrings.filter.push('setchannel');
  logOptions.substrings.filter.push('endchannel');
  // logOptions.substrings.filter.push('my_rolling_stats_update');
  logOptions.substrings.filter.push('forced_release');
  logOptions.substrings.filter.push('call_barge_ended');
  logOptions.substrings.filter.push('call_barge_result');
  logOptions.substrings.filter.push('call_monitor_ended');
  logOptions.substrings.filter.push('call_monitor_result');
  logOptions.substrings.filter.push('profile_count');
  logOptions.substrings.filter.push('agents_update');
  // logOptions.substrings.filter.push('profile_rstat_update');
  // logOptions.substrings.filter.push('queued_calls_update');
  // logOptions.substrings.filter.push('queue_rstat_update');
  // logOptions.substrings.filter.push('agent_rstat_update');

  logOptions.substrings.reject.push('rstat_update');
  logOptions.substrings.reject.push('rolling_stats_update');
  logOptions.substrings.reject.push('live_stats');

  // change this to change log level
  // never logs pong
  var logPattern = logOptions.generateConf(logOptions.SUBSTR_REJECT);

  // export
  return {
    baseUrl: baseUrl,
    oaUrl: oaUrl,
    imPrebindUrl: imPrebindUrl,
    imBindUrl: imBindUrl,
    profileUrl: profileUrl,
    staticUrl: staticUrl,
    wsUrl: wsUrl,
    reportsReachUrl: reportsReachUrl,
    reportsAuthUrl: reportsAuthUrl,
    gadgets: gadgets,
    activeFeatures: activeFeatures,
    pingInterval: pingInterval,
    pongWait: pongWait,
    gridUnitX: gridUnitX,
    gridUnitY: gridUnitY,
    gridMarginX: gridMarginX,
    gridMarginY: gridMarginY,
    logPattern: logPattern
  };
});
