define([
  'knockout',
  'models/timer',
  'models/gadget',
  'models/openacd',
  'json!../meta/agent-state.json',
  'text!../templates/agent-state.html'
], function (ko, Timer, Parent, oa, meta, template) {
  var AgentStateVM = function () {
    this.timer = new Timer();
    this.releaseReasons = ko.computed(function () {
      var ro = _.map(oa.releaseOptions(), function (opt) {
        return {
          label: opt.name,
          value: opt.id
        };
      });

      return _.union({
        label: 'Default',
        value: 'default'
      }, ro);
    });
    this.sessionStateText = ko.computed(function () {
      return oa.sessionStateText();
    });
    this.sessionStateStyle = oa.sessionStateIconStyle;

    this.releasedSince = ko.observable(new Date());
    this.lastReleaseReason = ko.observable(null);
    this.selectedReleaseReason = ko.observable();

    this.isProfileVisible = ko.observable(false);
    this.isEditingProfile = ko.observable(false);
    this.isViewingProfile = ko.computed(function () {
      return !this.isEditingProfile();
    }, this);

    this.profilePic = oa.profilePic;
    this.firstName = oa.firstName;
    this.lastName = oa.lastName;
    this.fullName = oa.fullName;
    this.position = oa.position;
    this.manager = oa.manager;
    this.phoneInternal = oa.phoneInternal;
    this.phoneDirect = oa.phoneDirect;
    this.phoneCell = oa.phoneCell;
    this.email = oa.email;
    this.homeAddressStreet = oa.homeAddressStreet;
    this.homeAddressCity = oa.homeAddressCity;
    this.homeAddressState = oa.homeAddressState;
    this.homeAddressCountry = oa.homeAddressCountry;
    this.homeAddressZip = oa.homeAddressZip;
    this.homeAddressText = oa.homeAddressText;
    this.area = oa.area;

    this.editables = ['phoneDirect', 'phoneCell', 'email', 'homeAddressStreet', 'homeAddressCity', 'homeAddressState', 'homeAddressCountry', 'homeAddressZip'];

    this.edit = {};
    _.each(this.editables, function (ed) {
      this.edit[ed] = ko.observable();
    }, this);

    this.skills = ko.observableArray();
    this.skillsText = ko.computed(function () {
      return this.skills().join(', ');
    }, this);
    this.clients = ko.observableArray();
    this.clientsText = ko.computed(function () {
      return this.clients().join(', ');
    }, this);

    this.isReleased = ko.computed(function () {
      return oa.isReleased();
    });
    this.isAvailable = ko.computed(function () {
      return !oa.isReleased();
    });

    this.canChangeReleaseState = ko.computed(function () {
      return oa.isConnected() && !oa.realeaseStateChanging();
    }, this);

    this.canRingTest = ko.computed(function () {
      return oa.isConnected() && !oa.realeaseStateChanging();
    });

    this.releaseText = ko.computed(function () {
      return oa.isReleased() ? 'Released' : 'Available';
    });

    this.lastReleaseReasonLabel = ko.computed(function () {
      var lrr = this.lastReleaseReason();
      return lrr !== null ? lrr['label'] : 'Default';
    }, this);

    this.releasedSinceStr = ko.computed(function () {
      var d = this.releasedSince();
      return d.format('dd mmm yyyy, hh:MM TT');
    }, this);

    // Stats
    this.myCptText = ko.computed(function () {
      var myCpt = oa.myCpt();
      if (!_.isFinite(myCpt)) return '--';

      return Timer.prototype.utils.formatMMSS(Math.round(myCpt));
    }, this);
    this.callsText = ko.observable('--');
    this.agentsText = ko.observable('--');
    this.cptText = ko.observable('--');

    this.profile = ko.observable();
    this.node = ko.observable();

    // UI callbacks
    this.toggleProfileVisible = function () {
      var b = this.isProfileVisible();
      this.isProfileVisible(!b);
    };

    this.toggleReleaseState = function () {
      if (oa.isReleased()) {
        oa.setAvailable();
      }
      else {
        oa.setReleased();
      }

      return false;
    };

    this.testRing = function () {
      oa.doRingTest();
      return false;
    };

    this.profilePicLoaded = function (vm, e) {
      $(e.currentTarget).parent().css('visibility', 'visible');
    };

    this.profilePicError = function (vm, e) {
      $(e.currentTarget).hide();
      $(e.currentTarget).parent().css('visibility', 'visible');
    };

    this.enableEditProfile = function () {
      this.isEditingProfile(true);

      _.each(this.editables, function (ed) {
        this.edit[ed](this[ed]());
      }, this);

      return false;
    };

    this.cancelEditButton = function () {
      oa.soundNotificationCheckValue(oa.soundNotificationCheckValueOld());
      oa.soundNotificationInterval(oa.soundNotificationIntervalOld());
      this.isEditingProfile(false);

      return false;
    };

    this.saveContactInfo = function () {
      var contactInfo = {
        'phone': {
          'direct': this.edit.phoneDirect(),
          'cell': this.edit.phoneCell()
        },
        'email': this.edit.email(),
        'home_address': {
          'street': this.edit.homeAddressStreet(),
          'city': this.edit.homeAddressCity(),
          'state': this.edit.homeAddressState(),
          'country': this.edit.homeAddressCountry(),
          'zip': this.edit.homeAddressZip()
        }
      };

      oa.setContactInfo(contactInfo, function (result) {
        this.isEditingProfile(false);
        if (_.isObject(result.error)) {
          notify('Unable to update your profile', 'error');
        }
        else {
          $.oucUtils.saveValueFromLocalForUser('soundNotificationCheck', oa.soundNotificationCheckValue());
          $.oucUtils.saveValueFromLocalForUser('soundNotificationInterval', oa.soundNotificationInterval());
          oa.soundNotificationCheckValueOld(oa.soundNotificationCheckValue());
          oa.soundNotificationIntervalOld(oa.soundNotificationInterval());
          oa.getContactInfo(null, null, true);
        }
      }, this);

      return false;
    };

    // properties
    this.getConnectionInfo = function () {
      oa.getConnectionInfo(function (info) {
        // Node
        this.node(info.node.replace(/^.*@/, ''));
        this.profile(info.profile);
        // Skills
        this.skills(oa.filterBasicSkills(info.skills));
        this.clients(oa.filterCsgSkills(info.skills));
      }, this);
    };

    // to be called on gadget init
    this.initialize = function () {
      // subscriptions
      oa.isReleased.subscribe(function (r) {
        if (r) {
          this.timer.reset();
        }
        else {
          this.timer.start();
        }
      }, this);

      amplify.subscribe(oa.events.RECEIVEDRELEASEDSTATE, this, function (obj) {
        this.releasedSince(new Date(obj['changeTime']));
        this.lastReleaseReason(obj['releaseData']);
        this.selectedReleaseReason(null);
      });

      this.fetchInit();
    };

    this.reset = function () {
      this.releasedSince(new Date());
      this.lastReleaseReason(null);
      this.selectedReleaseReason(null);

      this.isProfileVisible(false);
      this.isEditingProfile(false);

      _.each(this.editables, function (ed) {
        this.edit[ed](null);
      }, this);

      this.skills([]);
      this.clients([]);

      this.profile(null);
      this.node(null);

      this.fetchInit();
    };

    this.fetchInit = function () {
      this.getConnectionInfo();
      oa.getContactInfo();
      oa.getReleaseOptions();
    };

    this.selectedReleaseReason.subscribe(function (val) {
      if (val !== 'default') {
        oa.setReleased(val);
      }
      else {
        oa.setReleased();
      }
    });
  };

  var bindings = {
    // My Profile
    asSessionState: function () {
      return {
        text: this.sessionStateText,
        css: this.sessionStateStyle
      };
    },
    asProfileIcon: function () {
      return {
        click: this.toggleProfileVisible,
        css: { 'on': this.isProfileVisible }
      };
    },
    asProfilePic: function () {
      return {
        attr: {
          src: this.profilePic
        },
        event: {
          load: this.profilePicLoaded,
          error: this.profilePicError
        }
      };
    },
    asProfilePicFrame: function () {
      return {
        css: {
          'indented': this.isEditingProfile
        }
      };
    },
    asNameText: function () {
      return {
        text: this.fullName
      };
    },
    asPositionText: function () {
      return {
        text: this.position
      };
    },
    asAreaText: function () {
      return {
        text: this.area
      };
    },
    asManagerText: function () {
      return {
        text: this.manager
      };
    },
    asPhoneInternalText: function () {
      return {
        text: this.phoneInternal,
        'textNone': true
      };
    },
    asPhoneDirectText: function () {
      return {
        text: this.phoneDirect,
        'textNone': true
      };
    },
    asPhoneCellText: function () {
      return {
        text: this.phoneCell,
        'textNone': true
      };
    },
    asEmailText: function () {
      return {
        text: this.email,
        'textNone': true
      };
    },

    asPhoneDirectInput: function () {
      return {
        value: this.edit.phoneDirect
      };
    },
    asPhoneCellInput: function () {
      return {
        value: this.edit.phoneCell
      };
    },
    asEmailInput: function () {
      return {
        value: this.edit.email
      };
    },

    asHomeAddressText: function () {
      return {
        text: this.homeAddressText,
        'textNone': true
      };
    },

    asHomeAddressStreetInput: function () {
      return {
        value: this.edit.homeAddressStreet
      };
    },
    asHomeAddressCityInput: function () {
      return {
        value: this.edit.homeAddressCity
      };
    },
    asHomeAddressStateInput: function () {
      return {
        value: this.edit.homeAddressState
      };
    },
    asHomeAddressCountryInput: function () {
      return {
        value: this.edit.homeAddressCountry
      };
    },
    asHomeAddressZipInput: function () {
      return {
        value: this.edit.homeAddressZip
      };
    },
    asSoundNotificationCheckboxInput: function() {
      return  {
        checked: oa.soundNotificationCheckValue, //this.edit.soundNotificationCheckValue,//ctx.$root.checkedAutologout, //set default value for checkbox
        click: function() {
          //this.amAutologoutInterval(); //ctx.$root.amAutologoutInterval();
          //this.oa.storeSoundNotificationSettings();
          return true;
        }
      };
    },
    asSoundNotificationCheckbox: function() {
      return  {
        text: oa.soundNotificationCheckValue/*,
        'textNone': true*/
      };
    },
    amSoundNotificationInterval: function() {
      return {
        //enable: this.checkedSoundNotification, //ctx.$root.checkedAutologout,
        //visible: this.checkedSoundNotification,//this.checkedAutologout,
        value: oa.soundNotificationInterval, //this.edit.soundNotificationInterval, // ctx.$root.number,
        event: {
          'keydown': function(data, event, allBindingsAccessor){
            // Allow: backspace, delete, tab, escape, and enter
            if (event.keyCode == 46 || event.keyCode == 8 || event.keyCode == 9 || event.keyCode == 27 /*|| event.keyCode == 13*/ ||
              // Allow: Ctrl+A
              (event.keyCode == 65 && event.ctrlKey === true) ||
              // Allow: home, end, left, right
              (event.keyCode >= 35 && event.keyCode <= 39)) {
              // let it happen, don't do anything
              return true;
            }
            else {
              // Ensure that it is a number and stop the keypress
              if (event.shiftKey || (event.keyCode < 48 || event.keyCode > 57) && (event.keyCode < 96 || event.keyCode > 105) || (oa.soundNotificationInterval() < 1 && event.keyCode == 96 ) ){
                  event.preventDefault();
              }
              /* if (event.keyCode == 13) {
                //
                this.amAutologoutInterval(); //ctx.$root.amAutologoutInterval();
              }*/
              //this.oa.storeSoundNotificationSettings();
              return true;
            }
          }
        },

        valueUpdate: 'afterkeydown'
      };
    },
    asViewProfileEl: function () {
      return {
        visible: this.isViewingProfile,
        css: {
          'indented': this.isEditingProfile
        }
      };
    },
    asEditProfileEl: function () {
      return {
        visible: this.isEditingProfile
      };
    },

    asEditProfileButton: function () {
      return {
        click: this.enableEditProfile
      };
    },
    asCancelEditButton: function () {
      return {
        click: this.cancelEditButton
      };
    },
    asSaveProfileButton: function () {
      return {
        click: this.saveContactInfo
      };
    },

    asSkillsText: function () {
      return {
        text: this.skillsText,
        'textNone': true
      };
    },
    asClientsText: function () {
      return {
        text: this.clientsText,
        'textNone': true
      };
    },

    asProfileDiv: function () {
      return {
        css: { 'editable-box': this.isEditingProfile }
      };
    },

    asProfileMoreInfoDiv: function () {
      return {
        visible: this.isProfileVisible,
        css: { 'editing': this.isEditingProfile }
      };
    },

    // Current Session
    asNodeText: function () {
      return {
        text: this.node,
        'textNone': true
      };
    },
    asProfileText: function () {
      return {
        text: this.profile,
        'textNone': true
      };
    },

    asVisibleIfReleased: function () {
      return {
        visible: this.isReleased
      };
    },
    asVisibleIfAvailable: function () {
      return {
        visible: this.isAvailable
      };
    },
    asReleaseButton: function () {
      return {
        enable: this.canChangeReleaseState,
        click: this.toggleReleaseState
      };
    },
    asReleaseButtonIcon: function () {
      return {
        css: {
          'icon-play': this.isReleased,
          'icon-stop': this.isAvailable
        }
      };
    },
    asRingTestButton: function () {
      return {
        enable: this.canRingTest,
        click: this.testRing
      };
    },
    asReleaseStateLabel: function () {
      return {
        text: this.releaseText
      };
    },
    asReleaseReasonLabel: function () {
      return {
        text: this.lastReleaseReasonLabel
      };
    },
    asReleasedSinceLabel: function () {
      return {
        text: this.releasedSinceStr
      };
    },
    asReleaseReasonDropdown: function () {
      return {
        options: this.releaseReasons,
        optionsText: 'label',
        optionsValue: 'value',
        optionsCaption: 'Reason',
        value: this.selectedReleaseReason
      };
    },
    asMyCptText: function () {
      return {
        text: this.myCptText
      };
    },
    asCptText: function () {
      return {
        text: this.cptText
      };
    },
    asCallsText: function () {
      return {
        text: this.callsText
      };
    },
    asAgentsText: function () {
      return {
        text: this.agentsText
      };
    },
    asAgentTimerText: function () {
      return {
        text: this.timer.toHHMMSS
      };
    }
  };

  ko.bindingProvider.instance.registerBindings(bindings);

  return {
    ViewModel: AgentStateVM,
    Parent: Parent,
    meta: meta,
    template: _.template(template, {})
  };
});
