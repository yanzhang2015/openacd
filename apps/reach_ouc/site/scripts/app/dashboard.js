
define([
  'knockout',
  'config',
  'models/openacd',
  'viewModels/header',
  'viewModels/nav',
  'viewModels/widgetAdd'
], function (ko, config, oa, header, nav, widgetAdd) {
  OASLEEP = null;

  var $el = $('#dcontent');
  var gadgets = [];
  var isUnloading = false;
  var hasWebSocketError = false;
  var resizeThrottle = {};
  var tabGrids = {};
  var staticRoot = config.staticUrl;

  $.oucUtils.storeLocallyForUser = function (key, value) {
    var parentObj = $.oucUtils.getValueFromLocal(oa.username()) || {};
    parentObj[key] = value;
    $.oucUtils.storeLocally(oa.username(), parentObj);
  };
  $.oucUtils.getValueFromLocalForUser = function (key) {
    var parentObj = $.oucUtils.getValueFromLocal(oa.username()) || {};
    return parentObj[key];
  };

  // get notifications permission
  $.oucUtils.requestNotificationPermission();

  // openacd connection related
  oa.connect();

  $(window).on('beforeunload', function (evt) {
    var isConnected = oa.isConnected();
    var hasChannel = oa.isProcessing();
    if (isConnected) {
      if (hasChannel) {
        var msg = 'You are currently processing a request and may lose unsaved data.';
        if (_.isUndefined(evt)) {
          evt = window.event;
        }

        oa.sleep();
        timedCount();
        isUnloading = true;

        if (evt) {
          evt.returnValue = msg;
        }
        return msg;
      }
      else {
        isUnloading = true;
        oa.disconnect();
      }
    }
  });

  amplify.subscribe(oa.events.WEBSOCKETERROR, function (d) {
    console.log('websocket_err')
    hasWebSocketError = true;
    reconnect('Unable to establish a connection to the server.');
  });

  amplify.subscribe(oa.events.CONNECTED, function () {
    nav.applyLocalSettings();

    oa.getPermissionProfile(function (result){
      _.each( result, function (x) {
        //console.log("!!agent manager queue lines:"+bla['queue']);
        oa.permissionsQueues(x['queues']);
        oa.permissionsAgents(x['agents']);
        oa.permissionsMonitor(x['monitor']);
        oa.permissionsBarge(x['barge']);
        oa.permissionsTransferToQueue(x['trfq']);
        oa.permissionsConfToQueue(x['confq']);
        oa.permissionsTransferToAgent(x['trfa']);
        oa.permissionsConfToAgent(x['confa']);
        oa.permissionsTransferToNumber(x['trfn']);
        oa.permissionsConfToNumber(x['confn']);
        oa.permissionsTransferSkills(x['chgsktrf']);
        oa.permissionsControlAgentState(x['ctras']);
        oa.permissionsWidgets(x['wdg']);
        oa.permissionsCustomizeDesktop(x['custdsk']);
        oa.permissionsTabReports(x['rptab']);
        oa.permissionsTabSupervisor(x['sutab']);

        if(!oa.permissionsTabReports()){
          $('#reports-ui').hide();
          nav.hideTab("reports");
          if(window.location.hash == "#reports"){
            alert('You do not have the permission to access this page', function () {
              window.location.hash.replace('#', '');
              window.location.hash = '#main';
              window.location.reload();
            });
          }
        }
        if(!oa.permissionsTabSupervisor()){
          nav.hideTab("supervisor");
          if(window.location.hash == "#supervisor"){
            alert('You do not have the permission to access this page', function () {
              window.location.hash.replace('#', '');
              window.location.hash = '#main';
              window.location.reload();
            });
          }
        }
      });
      initWidgetAdd();

      //remove x button and change move to default cursor in case customize desktop is unchecked
      if(oa.permissionsCustomizeDesktop() == false){
        $('.gadget-icons').hide();
        $('.gadget-header').attr({style: "cursor: default"});
      }
      else{
        $('.session-manager-gadget').children('.gadget-header').children('.gadget-icons').hide();
        $('.agent-state-gadget').children('.gadget-header').children('.gadget-icons').hide();
      }
    });

    oa.getContactInfo(function (info) {
      if (info['first_name']) {
        header.welcomeName(info['first_name']);
      }
      else if (info['last_name']) {
        header.welcomeName(info['last_name']);
      }
    });

    //load sound notification stuff after agent is connected, so oa.username is set to correct value
    $.oucUtils.saveValueFromLocalForUser = function (key, value) {
      var parentObj = $.oucUtils.getValueFromLocal(oa.username()) || {};
      parentObj[key] = value;
      $.oucUtils.storeLocally(oa.username(), parentObj);
    };

    $.oucUtils.loadValueFromLocalForUser = function (key) {
      var parentObj = $.oucUtils.getValueFromLocal(oa.username()) || {};
      return parentObj[key];
    };

    if($.oucUtils.loadValueFromLocalForUser('soundNotificationCheck') != null)
      oa.soundNotificationCheckValue = ko.observable($.oucUtils.loadValueFromLocalForUser('soundNotificationCheck'));
    if($.oucUtils.loadValueFromLocalForUser('soundNotificationInterval') != null)
      oa.soundNotificationInterval = ko.observable($.oucUtils.loadValueFromLocalForUser('soundNotificationInterval'));



    _.each(gadgets, function (g) {
      if (!g.isInit) {
        g.initialize();
      }
      else {
        if (oa.getBaseUrlStatus() === 0) g.reset(); // means just reconnected
      }
    });

    if (oa.getBaseUrlStatus() !== 1) {
      $('#banner').slideUp();
    }

    $('.dashboard-loader').hide();
    oa.setBaseUrls();
  });


  amplify.subscribe(oa.events.DISCONNECTED, function (e) {
    console.log('ondisconnect', hasWebSocketError)
    if (!isUnloading) {
      if (e.replaced) {
        alert('The server has terminated this session because your account has been logged in somewhere else.', function () {
          $.oucUtils.clearSessionValues();
          window.location = header.logoutLink;
        });
      }
      else if (e.kicked) {
        $.oucUtils.removeSessionCookie();
        $.oucUtils.clearSessionValues();
        alert('You have been logged off by a supervisor.', function () {
          window.location = header.logoutLink;
        });
      }
      else {
        if (hasWebSocketError) {
          // filter onclose due to websocket error
          hasWebSocketError = false;
        }
        else {
          reconnect('The server has terminated your session.');
        }
      }
    }
  });

  amplify.subscribe(oa.events.FORCEDRELEASE, function (d) {
    if (d.data.code === '4001') {
      oa.soundNotificationPlay();
      alert('Your status was set to released. Please check your phone connection.', function() {
        oa.clearSoundNotification();
      });

      if (document.hidden || document.webkitHidden) {
        $.oucUtils.alertDesktop('Your status was set to released!', 'Please check your phone connection.');

        $.oucUtils.blinkTitleBar('Your status was set to released!');
        $(document).one('visibilitychange webkitvisibilitychange', function () {
          $.oucUtils.restoreTitleBar();
        });
      }
    }
  });

  $el.find('div.no-grid').each(function () {
    var vm =  $(this).parent('div.tab-pane').attr('id').replace('-tab', '');
    if (config.activeFeatures[vm]) {
      require(['viewModels/' + vm], function () {
         // console.log(vm + ' ready')
      });
    }
    else {
      nav.hideTab(vm);
    }
  });

  function timedCount() {
    OASLEEP = setTimeout(timedCount, 1000);
    if (isUnloading) {
      // console.log('stayed on page')
      isUnloading = false;
      oa.startPing();
      clearTimeout(OASLEEP);
    }
  }

  function reconnect(failMsg) {
    if (oa.getBaseUrlStatus() === 1) {

    console.log('baseUrl status 1', oa.baseUrl)
      oa.setBaseUrlStatus(0);

      $('.dashboard-loader').show();
      notify('Reconnecting...', 'info', 600000);

      oa.reset();
      setTimeout(function () {
        console.log('connecting to ' + oa.baseUrl);
        oa.connect();
      }, 2000);
    }
    else if (oa.getBaseUrlStatus() === 0) {

    console.log('baseUrl status 0', oa.baseUrl)
      oa.setBaseUrlStatus(-1);

      var isUrlUpdated = oa.setBaseUrl();
      if (isUrlUpdated) {
        setTimeout(function () {
          console.log('connecting to ' + oa.baseUrl);
          oa.connect();
        }, 2000);
      }
      else {
        console.log('baseUrl status -1')
        alert(failMsg, function () {
          $.oucUtils.clearSessionValues();
          window.location = header.logoutLink;
        });
      }
    }
  }

  // gridster dashboard related
  $el.find('div.gridster').each(function () {
    var tab = $(this).parent('div.tab-pane').attr('id').replace('-tab', '');
    var grid = $(this).find('ul.gridster-wrapper').gridster({
      widget_margins: [config.gridMarginX, config.gridMarginY],
      widget_base_dimensions: [config.gridUnitX, config.gridUnitY],
      min_cols: 3,
      min_rows: 100,
      max_size_x: 3,
      max_size_y: 100,
      extra_rows: 100, // TODO: make this make sense
      widget_selector: 'li.gridster-widget',
      draggable:{
        handle: '.gadget-header, .gadget-header h3',
        start: function (event, ui, widget) {
          this.$player.css('zIndex', 4);
        },
        stop: function (event, ui, widget) {
          this.$player.css('zIndex', 3);
          this.saveLayout();
        }
      },
      serialize_params: function (widget, wgd) {
        var obj = {};
        obj['name'] = widget.find('.gadget').attr('data-gadgetid').replace('-gadget', '').replace('-', ' ');
        obj['position'] = [wgd.col, wgd.row];
        obj['width'] = wgd.size_x;
        return obj;
      }
    }).data('gridster');

    tabGrids[tab] =  $.extend(grid, {
      layout: $(this).hasClass('wide') ? 'wide' : 'narrow',
      isOnCurrentTab: function () {
        return nav.getCurrentTab() === tab;
      },
      setWideLayout: function () {
        this.layout = 'wide';
        this.$wrapper.addClass('wide').removeClass('narrow');
        this.add_faux_cols(3);
        this.init();

        this.saveLayout();
      },
      setNarrowLayout: function () {
        this.layout = 'narrow';
        this.$wrapper.addClass('narrow').removeClass('wide');
        this.cols = 3;
        this.init();

        var grid = this;
        this.$widgets.each(function () {
          var col = parseInt($(this).attr('data-col'), 10),
              sizeX = parseInt($(this).attr('data-sizex'), 10),
              sizeY = parseInt($(this).attr('data-sizey'), 10),
              maxX = col + sizeX - 1;
          // console.log($(this).coords().grid)
          if (maxX > 3) {
            var np = grid.next_position(sizeX, sizeY);
            // console.log(np)
            $(this).attr('data-col', np.col);
            $(this).attr('data-row', np.row);
            $(this).coords().grid.col = np.col;
            $(this).coords().grid.row = np.row;
            grid.init();
          }
        });

        this.saveLayout();
      },
      saveLayout: function () {

        var gadgets = _(this.serialize()).chain().sortBy(function (gadget) {
          return gadget.position[0]; // col
        }).sortBy(function (gadget) {
          return gadget.position[1]; // row
        }).value();

        // console.log(tab, this.layout, JSON.stringify(gadgets))
        if(oa.permissionsCustomizeDesktop()){
          oa.saveTabLayout(tab, this.layout, gadgets);
        }
      }
    });
  });

  nav.onClickSwitchToNarrow = function () {
    var tab = nav.getCurrentTab(),
        grid = tabGrids[tab];
    grid.setNarrowLayout();
  };
  nav.onClickSwitchToWide = function () {
    var tab = nav.getCurrentTab(),
        grid = tabGrids[tab];
    grid.setWideLayout();
  };
  nav.isPinned.subscribe(function (isPinned) {
    if (isPinned) {
      $el.parent().addClass('offset-nav');
    }
    else {
      $el.parent().removeClass('offset-nav');
    }
  });

  $el.find('ul.gridster-wrapper').on('click', '.gridster-widget', function (e) {
    // bring active widget to top
    var li = $(this);
    $(li).css('zIndex', 3);
    $el.find('.gridster-widget').not(li).css('zIndex', 2);
  }).on('click', '.gadget-remove', function () {
    var gEl = $(this).parents('.gadget');
    //push widget to available widget list
    var gconf = config.gadgets[gEl.attr('data-gadgetid')];
    if (_.isObject(gconf)) {
      require([gconf['module']], function (module) {
        widgetAdd.availableWidgets.push(module.meta);
        widgetAdd.sortOptions();
      });
    }
    var widget = $(this).parents('.gridster-widget');
    var grid = getGagetGrid(gEl);
    var i = parseInt(gEl.attr('id').replace('gadget-', ''), 10);
    delete gadgets[i - 1];
    grid.remove_widget(widget, function () {
      // callback
      grid.saveLayout();
    });
  });

  // lazy load gadget modules
  /*$el.find('.gadget').each(function () {
    gadgetify(this);
  }).resize(function () {
    // event delegation does not work with plugin
    onResize.call(this);
  });*/

  function deleteGadgets(){
    $el.find('.gadget').each(function () {
      var widget = $(this).parents('.gridster-widget');
      var grid = getGagetGrid($(this));
      //console.dir(grid);
      var i = parseInt($(this).attr('id').replace('gadget-', ''), 10);
      delete gadgets[i - 1];
      grid.remove_widget(widget, function () {
        // callback
        grid.saveLayout();
      });
    });
  }

  function loadGadgetModule(myGadgets, myId)
  {
    if( myId > _.size(myGadgets) ) {
       // load gadget modules from cache
      if(oa.permissionsCustomizeDesktop()){
        $el.find('.gadget').each(function () {
          gadgetify(this);
        }).resize(function () {
          // event delegation does not work with plugin
          onResize.call(this);
        });
      }
      $('.dashboard-loader').hide();
    }
    else{
      var gconf = _.where(myGadgets, {id: myId});
      myId++;
      if (gconf[0]['isActive'] ) {
        var match = gconf[0]['default'] || _.contains(oa.permissionsWidgets(), gconf[0]['name']);
        //add only default or selected widgets in permissionWidgets list
        if(!match) {
          loadGadgetModule(myGadgets, myId);
        }
        else{
          require([gconf[0]['module']], function (module) {
            if ((module.meta.supervisor && (oa.level() === 'supervisor')) || !module.meta.supervisor) {
              //in case customize desktop is unchecked we need to load all widgets selected in permissionWidgets and default
              if (oa.permissionsCustomizeDesktop() == false) {
                //load widget in correct tab
                var grid;
                if (gconf[0]['supervisorTab'])
                  grid = tabGrids["supervisor"];
                else
                  grid = tabGrids["main"];

                  addWidget(grid, module.meta);
                  loadGadgetModule(myGadgets, myId);
              }
              //add widget to selectable box only in case customize desktop is checked
              else {
                widgetAdd.availableWidgets.push(module.meta);
                widgetAdd.sortOptions();
                loadGadgetModule(myGadgets, myId);
              }
            }
          });
        }
      }
    }
  }

  function initWidgetAdd() {
    if (nav.isAddEnabled()) return; // means already initialized
    if (!oa.isConnected()) return;
    //in this case we neeed to delete all gadgets saved in cache and load only gagdets selected in reach config
    if (!oa.permissionsCustomizeDesktop()) deleteGadgets();

    $('.dashboard-loader').show();
    loadGadgetModule(config.gadgets, 1);

    nav.isAddEnabled(true);
    nav.onClickAddWidget = function () {
      var tab = nav.getCurrentTab(),
          grid = tabGrids[tab];
      widgetAdd.grid = grid;
      widgetAdd.$el.modal('show');
    };

    widgetAdd.$el.modal({
      'show': false
    });
    widgetAdd.widetAddHandler =  function (meta) {
      addWidget(widgetAdd.grid, meta); //id
    };
  }

  function onResize() {
    // on gadget resize, resize parent widget on grid
    // bind to resize event before adding gadgets
    var id = $(this).attr('id');
    var grid = getGagetGrid(this);

    // console.log(id, grid.isOnCurrentTab())
    // only watch out for resize of visible gadgets
    if (!grid.isOnCurrentTab()) return;

    grid.disable();
    try {
      clearTimeout(resizeThrottle[id]);
    } catch (e) {}

    var gadget = this;
    resizeThrottle[id] = setTimeout(function () {
      resizeWidget(grid, gadget);
    }, 300); // because 400 is default animation speed and resize poll is 250
  }

  function gadgetify(container) {
    if (oa.isConnected() == false) return;
    var gconf = config.gadgets[$(container).attr('data-gadgetid')];
    if (_.isObject(gconf)) {
      require([gconf['module']], function (module) {
        //load only widgets which are allowed from permissionsWidgets or default one
        var match = _.contains(oa.permissionsWidgets(), gconf['name']) || gconf['default'];
        if(!match) return;

        //remove loaded widgets from available widgets list as its displayed on dashboard
        widgetAdd.availableWidgets.remove(module.meta);
        widgetAdd.sortOptions();

        function G() {
          module.Parent.call(this, module.meta, (new module.ViewModel()), module.template);
        }
        G.prototype = module.Parent.prototype;
        var g = new G();
        var li = g.render(container, staticRoot + module.meta.stylesheet).parent();

        if (oa.isConnected()) {
          g.initialize();
        }

        var grid = getGagetGrid(g.$el),
        sizeX = g.meta.size_x,
        sizeY = getSizeY(g.openHeight);
        // this make error if customize desktop was unchecked
        //grid.resize_widget(li, sizeX, sizeY);

        // pre-expand the widget for user initiated restore
        g.attachCallback('beforeSlideUp', function () {
          this.openHeight = g.$el.height();
        });
        g.attachCallback('beforeSlideDown', function () {
          grid.resize_widget(li, sizeX, getSizeY(this.openHeight));
        });

        gadgets.push(g);
        g.$el.attr({
          id: 'gadget-' + gadgets.length
        });

      });
    }
  }

  function getGagetGrid(gadget) {
    var tab = $(gadget).parents('.tab-pane').attr('id').replace('-tab', '');
    return tabGrids[tab];
  }

  function getSizeY(height) {
    return Math.ceil((height + (config.gridMarginY * 2)) / (config.gridUnitY + (config.gridMarginY * 2)));
  }

  function addWidget(grid, gadgetMeta) {
    var data = $('#container-gs_w-dummy').html();
    //use first node
    var initY = getSizeY(gadgetMeta.init_height);
    var li = grid.add_widget($(data)[0], gadgetMeta.size_x, initY);
    var gadget = li.find('.gadget');
    gadget.attr({
      'data-gadgetid': gadgetMeta.id
    });
    gadgetify(gadget);
    gadget.resize(function () {
      // bind event handlers when adding a new widget
      onResize.call(this);
    });
    li.show();

    //$('.dashboard-loader').show();
    setTimeout(function () {
      $(document).scrollTop(li.offset().top);
      $(document).scrollLeft(li.offset().left);
      //$('.dashboard-loader').hide();
      grid.saveLayout();
    }, 350); // save after resize
  }

  function resizeWidget(grid, gadget) {

    var height = $(gadget).height(),
    li = $(gadget).parent(),
    oldY = parseInt(li.attr('data-sizey'), 10),
    sizeY = getSizeY(height),
    sizeX = parseInt(li.attr('data-sizex'), 10);

    // check if resize is neccessary
    if (sizeY === oldY) {
    if(oa.permissionsCustomizeDesktop())
      grid.enable();
      return;
    }

    // console.log(li, sizeX, sizeY)
    grid.resize_widget(li, sizeX, sizeY);
    if(oa.permissionsCustomizeDesktop())
    grid.enable();
  }
});
