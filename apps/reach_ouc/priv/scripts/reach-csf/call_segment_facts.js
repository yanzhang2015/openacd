#!/usr/bin/env node
  
var fs = require('fs');
var _ = require('underscore');
var program = require('commander');
var mongoose = require('mongoose');
var ini = require('ini');
var namespace = 'CSF: ' + new Date().toUTCString();
var logger = require('minilog')(namespace);
//require('minilog').pipe(fs.createWriteStream('/var/log/sipxpbx/reach-call-segment-facts.log', {flags: 'a'}));
require('minilog').enable()

// RUN ONLY ON PRIMARY MONGO NODE
var sys = require('sys')
var exec = require('child_process').exec;
function primaryMongo(error, stdout, stderr) {
  stdout = stdout.replace(/(?:\r\n|\r|\n)/g, '');
  if (stdout.toString()==='true') {
    logger.info('Mongo node is primary, proceeding ...');
  } else {
    logger.info('Mongo node is not primary, terminating ...');
    process.exit(0);
  }
}
exec("mongo --quiet $SRC --eval 'db.isMaster().ismaster;' 2>/dev/null", primaryMongo);

logger.info('Reading configuration ...');
var config = {
  mongoini: '/etc/sipxpbx/mongo-client.ini',
  db: 'reach_reports',
  collections: {
    event: 'events',
    callSegmentFact: 'call_segment_facts',
    activeCallSegment: 'active_call_segments',
    cdrMigrated: 'cdr_migrations'
  },
  schema: {
    eventSchema: {
      '_id': Object,
      'event_finished': Object,
      'event_count': Number,
      'from': Object,
      'to': Object,
      'event_starting': Object,
      'reason': String,
      'call_id': String,
      'original_id': String,
      'call_segment_count': String,
      'call_type': String,
      'media_type': String,
      'direction': String,
      'source_module': String,
      'original_skills': Object,
      'node': String,
      'caller_id': String,
      'caller_ani': String,
      'client_id': String,
      'client_label': String,
      'line': String,
      'line_name': String,
      'line_extension': String,
      'line_queue': String,
      'queue': String,
      'queue_group': String,
      'skills_required': Object,
      'media_id': String,
      'agent_login': String,
      'agent_firstname': String,
      'agent_lastname': String,
      'agent_inherent_skills': Object,
      'agent_profile': String,
      'agent_profile_skills': Object,
      'agent_effective_skills': Object,
      'agent_node': String,
      'final_skills': Object,
      'transfer_to': String,
      'ended_by': String,
      'call_segment_ended': Number,
      'vsn': Number
    },
    callSegmentFactSchema: {
      '_id': Object,
      'value': Object
    },
    activeCallSegmentSchema: {
      'call_id': String,
      'call_segment_count': String,
      'data': Object
    },
    cdrMigratedSchema: {
      'migrated': Boolean,
      'version': String
    }
  },
  end: new Date(),
  version: "14.10"
};

try {
  var iniFile = ini.parse(fs.readFileSync(config.mongoini, 'utf-8'));
  var connectionUrl = iniFile.connectionUrl.split('?')[0];
  config.mongo = {
    connectionUrl: connectionUrl + config.db
  };
}
catch (e) {
  config.mongo = {
    connectionUrl: 'mongodb://localhost/reach_reports'
  };
}

program
  .version('0.0.1')
  .usage('[options]')
  .option('-e, --end <end>', 'specify end timestamp of events to be covered', parseInt)
  .option('-u, --url <url>', 'specify mongodb connection URL')
  .parse(process.argv);

// override config with args
if (_.isFinite(program.end)) {
  config.end = new Date(program.end);
}

if (program.url) {
  config.mongo.connectionUrl = program.url;
}

logger.info('Mongo Connection Url:', config.mongo);

mongoose.connect(config.mongo.connectionUrl);
var db = mongoose.connection;
db.on('error', function(error){
    logger.error('Connection error: ', error);
});
db.once('open', function () {
  var Event = mongoose.model(config.collections.event, mongoose.Schema(config.schema.eventSchema));
  var CallSegmentFact = mongoose.model(config.collections.callSegmentFact, mongoose.Schema(config.schema.callSegmentFactSchema));
  var ActiveCallSegment = mongoose.model(config.collections.activeCallSegment, mongoose.Schema(config.schema.activeCallSegmentSchema));
  var CdrMigrated = mongoose.model(config.collections.cdrMigrated, mongoose.Schema(config.schema.cdrMigratedSchema));

  CdrMigrated.findOne(function(err, cdrMigrated) {
    if(err) {
        logger.error(err);
        process.exit(1);
    }

    if(_.isObject(cdrMigrated)) {
       logger.info("CDRs already migrated...");
       processEvents();
    } else {
       logger.info('Some CDRs are not migrated...');
       updateMigratedFlag();
    }
  });

  function updateMigratedFlag() {
    var cdrMigrated = new CdrMigrated({
      'migrated' : true,
      'version' : config.version,
    });
    cdrMigrated.save(function(err, cdrMigrated){
      if(err) {
        return logger.err(err);
      }
      logger.info('Updating CDR migrated flag...');
      processAllCsfs();
    });
  }

  function processAllCsfs() {
    var lastCsfDate = null;
    CallSegmentFact.findOne({'value.start_ts' : {'$ne' : null}}, {},
      {sort: { 'value.start_ts' : -1 }}, function (err, lastCsf) {
        if (err) {
          logger.error(err);
          process.exit(1);
        }
        if (_.isObject(lastCsf)) {
          lastCsfDate = lastCsf.value.start_ts;
          logger.info('Getting the last call segment fact start_ts: ' + lastCsfDate);
          processEvents(lastCsfDate);
        }
    });
  }

  function processEvents() {

    logger.info('Processing events...');
    var callSegments = {};
    var query = {};
    var queryFromObj = {};
    if(arguments.length == 1) {
        queryFromObj['$gt'] = arguments[0];
        query['from'] = queryFromObj;
    }

    ActiveCallSegment.find(function (err, acss) {
      if (err) {
        logger.error(err);
        process.exit(1);
      }
      ActiveCallSegment.remove(function (err) {
        if (err) return logger.error(err);
      });

      logger.warn(acss);
      _.each(acss, function (acs) {
        var callId = acs.call_id;
        var callSegmentCount = acs.call_segment_count;
        var callSegmentId = callId + '_' + callSegmentCount;
        var callSegment = new CallSegment(callId, callSegmentCount);
        callSegment.data = acs.data;
        callSegments[callSegmentId] = callSegment;
      });

      Event.find(query, {}, {sort: {'from' : 1}}, function (err, events) {
        if (err) {
          logger.error(err);
          process.exit(1);
      }

      Event.remove(function (err) {
        if (err) return logger.error(err);
      });

      if (events.length < 1) {
        logger.info('No new events...');
        process.exit(0);
      }

      _.each(events, function (event) {
        var callId = event.original_id;
        var callSegmentCount = event.call_segment_count;
        var callSegmentId = callId + '_' + callSegmentCount;

        var callSegment = callSegments[callSegmentId];
        if (_.isUndefined(callSegment)) {
          callSegment = new CallSegment(callId, callSegmentCount);
          callSegments[callSegmentId] = callSegment;
        }
        callSegment.handle(event);
      });

      var pendingWrites = 0;

      _.each(callSegments, function (callSegment) {
        if (!callSegment.isActive()) {
          var csf = callSegment.getSegmentFact();

          var callSegmentFact = new CallSegmentFact(csf);
          pendingWrites++;
          callSegmentFact.save(function (err, callSegmentFact) {
            pendingWrites--;
            if (err) return logger.error(err);
            logger.debug('Call Segment Fact: ', JSON.stringify(callSegmentFact));
            if (pendingWrites === 0) process.exit(0);
          });
        }
        else {
          var csJson = callSegment.getJson();

          var activeCallSegment = new ActiveCallSegment(csJson);
          pendingWrites++;
          activeCallSegment.save(function (err, activeCallSegment) {
            pendingWrites--;
            if (err) return logger.error(err);
            logger.debug('Active Call Segment Fact: ', JSON.stringify(activeCallSegment));
            if (pendingWrites === 0) process.exit(0);
            });
          }
        });
      });
    });
  }

  function CallSegment(callId, callSegmentCount) {
    this.callId = callId;
    this.callSegmentCount = callSegmentCount;

    this.data = {
      'inivr_ts': null,
      'start_ts': null,
      'end_ts': null,
      'line': null,
      'node': null,
      'origination': null,
      'origination_type': null,
      'client': null,
      'disposition': null,
      'disposition_auto': false,
      'caller_id': null,
      'caller_ani': null,
      'f_ring': 0,
      'ring_count': 0,
      'first_ring_ts': null,
      'last_ring_ts': null,
      'media_id': null,
      'queue_name': null,
      'queue_group': null,
      'wait_duration': null,
      'oncall_duration': null,
      'wrapup_duration' : null,
      'f_answer': 0,
      'answer_ts': null,
      'agent_login': null,
      'agent_firstname' : null,
      'agent_lastname' : null,
      'agent_profile': null,
      'new_call_segment': 0,
      'final_call_segment': 1,
      'from_transfer': 0,
      'to_transfer': 0,
      'transfer_to': null,
      'next_segment': null,
      'f_abandon': 0,
      'ended_by': null,
      'call_segment_ended': 0,
      'media_type': null,
      'direction': null,
      'original_skills': null,
      'final_skills': null,
      'agent_node': null
    };

    this.handle = function (event) {
      if (!('to' in event || 'from' in event)) {
        logger.warn('This event does not contain to or from fields: ' + JSON.stringify(event));
        return;
      }

      var eventDuration = (event.to - event.from) / 1000;
      switch (event.event_finished) {
      case 'inivr' :
        this.data.start_ts = event.from;
        this.data.inivr_ts = event.from;
      case 'inqueue':
      case 'remove_skills':
      case 'add_skills':
      case 'announce':
      case 'voicemail':
        this.data.start_ts = event.from;
        this.data.line = event.line || null;
        this.data.client = event.client_label || null;
        this.data.caller_id = event.caller_id || null;
        this.data.caller_ani = event.caller_ani || null;
        this.data.disposition = event.disposition || null;
        this.data.media_id = event.call_id || null;
        this.data.node = event.node || null;
        this.data.direction = event.direction || null;
        this.data.media_type = event.media_type || null;
        this.data.original_skills = event.original_skills || null;
        this.data.queue_name = event.queue || null;
        this.data.queue_group = event.queue_group || null;

        this.data.wait_duration += eventDuration;
        if (event.event_starting === 'hangup') {
          this.data.f_abandon = 1;
        }

        if (event.event_finished === 'voicemail') {
          this.data.disposition = 'voicemail';
          this.data.disposition_auto = true;
        }
        break;

      case 'ringing':
        this.data.f_ring = 1;
        this.data.ring_count = this.data.ring_count + 1;
        if (this.data.ring_count === 1) {
          this.data.first_ring_ts = event.from;
        }
        this.data.last_ring_ts = event.from;

        this.data.agent_login = event.agent_login || null;
        this.data.agent_firstname = event.agent_firstname || null;
        this.data.agent_lastname = event.agent_lastname || null;
        this.data.agent_profile = event.agent_profile || null;
        this.data.agent_node = event.agent_node || null;
        this.data.final_skills = event.final_skills || null;

        this.data.wait_duration += eventDuration;

        if (event.event_starting === 'hangup') {
          this.data.f_abandon = 1;
        }
        break;
      case 'oncall' :
        this.data.f_answer = 1;
        this.data.answer_ts = event.from;

        this.data.oncall_duration += eventDuration;
        break;
      case 'hangup' :
        this.data.end_ts = event.from;
        this.data.ended_by = event.ended_by;
        break;
      case 'stopped' :
        this.data.wrapup_duration = eventDuration;
        break;
      case 'agent_transfer':
      case 'queue_transfer':
      case 'outband_transfer':
        if (event.event_count !== 0) {
          this.data.to_transfer = 1;
          this.data.transfer_to = event.transfer_to;
          this.data.disposition = 'transfer';
          this.data.disposition_auto = true;
          this.data.final_call_segment = 0;
        }
        break;
      case 'agent_conference':
      case 'queue_conference':
        if (event.event_count !== 0) {
          this.data.transfer_to = event.transfer_to;
          this.data.disposition = 'conference';
          this.data.disposition_auto = true;
          this.data.final_call_segment = 0;
        }
        break;
      default:
        if (_.isObject(event.event_finished) && event.event_finished.media_custom === 'oncall') {
          this.data.oncall_duration += eventDuration;
        }
        break;
      }

      if (event.event_count === 0) {
        switch (event.event_starting) {
        case 'agent_transfer':
        case 'queue_transfer':
          this.data.origination_type = 'transfer';
          this.data.origination = event.agent_login;
          this.data.next_segment = event.transfer_destination;
          this.data.from_transfer = 1;
          break;
        case 'agent_conference':
        case 'queue_conference':
          this.data.origination_type = 'conference';
          this.data.origination = event.agent_login;
          this.data.next_segment = event.transfer_destination;
          break;
        }
      }

      if (event.call_segment_count == 1) { // String
        this.data.new_call_segment = 1;
        this.data.origination_type = 'line';
        this.data.origination = event.line;
      }

      if (event.call_segment_ended === 1) {
        this.data.call_segment_ended = 1;
      }
    };

    this.getSegmentFact = function () {
      return {
        '_id': {
          'call_id': this.callId,
          'call_segment_count': callSegmentCount,
          'vsn': 1
        },
        'value': this.data
      };
    };

    this.isActive = function () {
      return this.data.call_segment_ended !== 1;
    };

    this.getJson = function () {
      return {
        'call_id': this.callId,
        'call_segment_count': this.callSegmentCount,
        'data': this.data
      };
    };
  }
});
