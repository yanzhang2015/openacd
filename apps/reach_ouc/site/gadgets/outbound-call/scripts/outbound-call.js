define([
  'knockout',
  'models/timer',
  'models/gadget',
  'models/openacd',
  'json!../meta/outbound-call.json',
  'text!../templates/outbound-call.html'
], function (ko, Timer, Parent, oa, meta, template) {
  var OutboundCallVM = function () {
    this.sessionStateIconStyle = oa.sessionStateIconStyle;
    this.customerList = ko.computed(function () {
      var arr = _.map(oa.clients(), function (client) {
        return client.name;
      }).sort();
      arr.push('No Customer');
      return arr;
    });

    this.customer = ko.observable();
    this.destinationNumber = ko.observable();

    this.isInitiated = ko.computed(function () {
      return oa.outboundState() === 'initiated';
    });
    this.isAwaitingDestination = ko.computed(function () {
      return oa.outboundState() === 'awaiting_destination';
    });
    this.isOutgoingRinging = ko.computed(function () {
      return oa.outboundState() === 'outgoing_ringing';
    });
    this.isOncall = ko.computed(function () {
      return oa.outboundState() === 'oncall';
    });
    this.isEnded = ko.computed(function () {
      return oa.outboundState() === 'ended';
    });

    this.isOnHold = oa.isOutboundOnHold;

    this.canInitiateOutbound = ko.computed(function () {
      return !oa.isProcessing() && !oa.callingOutbound();
    }, this);
    this.isCustomerDropdownVisible = ko.computed(function () {
      return !oa.isProcessing() || (this.isOutgoingRinging() || this.isOncall());
    }, this);
    this.isDestinationEditable = ko.computed(function () {
      return !this.isOutgoingRinging() && !this.isOncall();
    }, this);
    this.isCallFormVisible = ko.computed(function () {
      return !this.isInitiated() && !this.isEnded();
    }, this);
    this.isCallControlsVisible = ko.computed(function () {
      return this.isOutgoingRinging() || this.isOncall();
    }, this);
    this.canCallOutbound = ko.computed(function () {
      var hasDestination = !!this.destinationNumber();
      return this.isAwaitingDestination() && hasDestination;
    }, this);

    this.timer = new Timer();
    this.holdTimer = new Timer();

    this.transConfNumber = ko.observable();
    this.transConf = ko.observable();
    this.transConfList = [{
      text: 'Transfer to number...',
      action: 'transfer'
    },
    {
      text: 'Conference to number...',
      action: 'conference'
    }];
    this.isTransConf = ko.computed(function () {
      return !!this.transConf();
    }, this);

    this.callOutbound = function () {
      if (!this.canCallOutbound()) return;

      var dest = this.destinationNumber();
      oa.outboundDestination = dest;
      oa.callOutbound(dest);
    };

    this.hangup = function () {
      if (oa.isProcessing()) {
        oa.hangup(oa.currentChannel());
      }
      else {
        oa.hangupOutbound();
      }
    };

    this.toggleHold = function () {
      var isOnHold = this.isOnHold();
      var isProcessing = oa.isProcessing();

      if (isOnHold && isProcessing) {
        oa.unhold(oa.currentChannel());
      }
      else if (isOnHold && !isProcessing) {
        oa.unholdOutbound();
      }
      else if (!isOnHold && isProcessing) {
        oa.hold(oa.currentChannel());
      }
      else {
        oa.holdOutbound();
      }
    };

    this.doTransConf = function () {
      var number = this.transConfNumber();
      var action = this.transConf();
      if (action === 'transfer') {
        oa.transferOutbound(number);
      }
      else if (action === 'conference') {
        oa.conferenceOutbound(number);
      }

      this.transConfNumber('');
      this.transConf('');
    };

    this.handleSessionState = function (ss) {
      if (ss === 'released' || ss === 'idle') {
        this.destinationNumber(null);
        this.customer(null);
      }
    };

    this.handleOutboundState = function (os) {
      switch (os) {
      case 'initiated':
        this.customer(oa.outboundCustomer);
        break;
      case 'awaiting_destination':
        this.customer(oa.outboundCustomer);
        break;
      case 'outgoing_ringing':
        this.customer(oa.outboundCustomer);
        this.destinationNumber(oa.outboundDestination);
        break;
      case 'oncall':
        this.customer(oa.outboundCustomer);
        this.destinationNumber(oa.outboundDestination);

        var answerMs = oa.getServerTime() - new Date(oa.outboundSessionStartTs).getTime();
        this.timer.isCountdown(false);
        this.timer.start(answerMs / 1000);
        this.holdTimer.isCountdown(false);
        this.holdTimer.start(answerMs / 1000);
        break;
      case 'ended':
        this.destinationNumber(null);
        this.customer(null);
        this.timer.reset();
        this.holdTimer.reset();
        break;
      default:
        break;
      }
    };

    this.initialize = function () {
      this.handleSessionState(oa.sessionState());
      this.handleOutboundState(oa.outboundState());
      oa.sessionState.subscribe(function (ss) { this.handleSessionState(ss); }, this);
      oa.outboundState.subscribe(function (os) { this.handleOutboundState(os); }, this);

      this.fetchInit();
    };

    this.reset = function () {
      this.customer();
      this.destinationNumber();

      this.fetchInit();
    };

    this.fetchInit = function () {
      oa.getClients();
    };

    this.customer.subscribe(function (customer) {
      if (!this.canInitiateOutbound() || !customer) return;

      oa.outboundCustomer = customer;
      oa.initiateOutbound(customer);
    }, this);

    this.isOnHold.subscribe(function (isOnHold) {
      this.holdTimer.reset();
      this.holdTimer.start(0);
    }, this);
  };

  var bindings = {
    ocSessionStateIcon: function () {
      return {
        css: this.sessionStateIconStyle
      };
    },
    ocVisibleWhenOncall: function () {
      return {
        visible: this.isOncall
      };
    },
    ocCustomerDropdownContainer: function () {
      return {
        visible: this.isCustomerDropdownVisible
      };
    },
    ocCustomerDropdown: function () {
      return {
        enable: this.canInitiateOutbound,
        options: this.customerList,
        value: this.customer,
        optionsCaption: 'Select a customer...'
      };
    },
    ocOutboundTimer: function () {
      return {
        text: this.timer.toMMSS
      };
    },
    ocCallForm: function () {
      return {
        visible: this.isCallFormVisible
      };
    },
    ocDestinationInput: function () {
      return {
        enable: this.isDestinationEditable,
        value: this.destinationNumber,
        valueUpdate: 'keyup'
      };
    },
    ocCallButton: function () {
      return {
        css: {
          'active': this.isOutgoingRinging
        },
        enable: this.canCallOutbound,
        click: this.callOutbound
      };
    },
    ocCallControls: function () {
      return {
        visible: this.isCallControlsVisible,
        css: {
          'hold wrap': this.isOnHold
        }
      };
    },
    ocHoldTimer: function () {
      return {
        text: this.holdTimer.toMMSS
      };
    },
    ocHoldButton: function () {
      return {
        enable: this.isOncall,
        css: {
          'active': this.isOnHold
        },
        click: this.toggleHold
      };
    },
    ocHangupButton: function () {
      return {
        click: this.hangup
      };
    },
    ocTransConfButton: function () {
      return {
        click: this.doTransConf
      };
    },
    ocTransConfSelect: function () {
      return {
        enable: this.isOncall,
        options: this.transConfList,
        value: this.transConf,
        optionsCaption: 'Select action...',
        optionsText: 'text',
        optionsValue: 'action'
      };
    },
    ocTransConfNumber: function () {
      return {
        visible: this.isTransConf,
        value: this.transConfNumber,
        valueUpdate: 'keyup'
      };
    }
  };

  ko.bindingProvider.instance.registerBindings(bindings);

  return {
    ViewModel: OutboundCallVM,
    Parent: Parent,
    meta: meta,
    template: _.template(template, {})
  };
});