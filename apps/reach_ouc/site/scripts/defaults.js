// global library customizations
// plugins
define([
  'knockout',
  'ko-classBindingProvider',
  'ko-mapping'
], function (ko, ClassBindingProvider, mapping) {
  /* =======================
   * KNOCKOUT CUSTOMIZATIONS
   * ======================= */

  ko.bindingProvider.instance = new ClassBindingProvider({
    fallback: true
  });

  ko.mapping = mapping;

  // If this is set, and the text of the element is null or an empty string, it is filled up with the text 'None' instead
  ko.bindingHandlers.textNone = {
    update: function (element, valueAccessor, allBindingsAccessor, viewModel) {
      if (!allBindingsAccessor().text) return;

      var el = $(element);
      var value = ko.utils.unwrapObservable(valueAccessor());
      var text = ko.utils.unwrapObservable(allBindingsAccessor().text);
      if (value && !text) {
        el.text('None').css({
          fontStyle: 'italic'
        });
      }
      else {
        el.css({
          fontStyle: ''
        });
      }
    }
  };

  // If this is set, and the text of the element is truncated, it gets a title attribute with the full text as value
  ko.bindingHandlers.titleOnTruncate = {
    update: function (element, valueAccessor, allBindingsAccessor, viewModel) {
      var el = $(element);
      var value = ko.utils.unwrapObservable(valueAccessor());
      var isEllipsisActive = (element.offsetWidth < element.scrollWidth);
      if (value && isEllipsisActive) {
        var text = el.text();
        el.attr('title', text);
      }
    }
  };

  // check value for linkable strings (url, mailto, ftp, etc.)
  // if linkable: wrap in <a>, else: use original text binding
  ko.bindingHandlers.linkyText = {
    update: function (element, valueAccessor) {
      var
        SCHEME = "[a-z\\d.-]+://",
        IPV4 = "(?:(?:[0-9]|[1-9]\\d|1\\d{2}|2[0-4]\\d|25[0-5])\\.){3}(?:[0-9]|[1-9]\\d|1\\d{2}|2[0-4]\\d|25[0-5])",
        HOSTNAME = "(?:(?:[^\\s!@#$%^&*()_=+[\\]{}\\\\|;:'\",.<>/?]+)\\.)+",
        TLD = "(?:ac|ad|aero|ae|af|ag|ai|al|am|an|ao|aq|arpa|ar|asia|as|at|au|aw|ax|az|ba|bb|bd|be|bf|bg|bh|biz|bi|bj|bm|bn|bo|br|bs|bt|bv|bw|by|bz|cat|ca|cc|cd|cf|cg|ch|ci|ck|cl|cm|cn|coop|com|co|cr|cu|cv|cx|cy|cz|de|dj|dk|dm|do|dz|ec|edu|ee|eg|er|es|et|eu|fi|fj|fk|fm|fo|fr|ga|gb|gd|ge|gf|gg|gh|gi|gl|gm|gn|gov|gp|gq|gr|gs|gt|gu|gw|gy|hk|hm|hn|hr|ht|hu|id|ie|il|im|info|int|in|io|iq|ir|is|it|je|jm|jobs|jo|jp|ke|kg|kh|ki|km|kn|kp|kr|kw|ky|kz|la|lb|lc|li|lk|lr|ls|lt|lu|lv|ly|ma|mc|md|me|mg|mh|mil|mk|ml|mm|mn|mobi|mo|mp|mq|mr|ms|mt|museum|mu|mv|mw|mx|my|mz|name|na|nc|net|ne|nf|ng|ni|nl|no|np|nr|nu|nz|om|org|pa|pe|pf|pg|ph|pk|pl|pm|pn|pro|pr|ps|pt|pw|py|qa|re|ro|rs|ru|rw|sa|sb|sc|sd|se|sg|sh|si|sj|sk|sl|sm|sn|so|sr|st|su|sv|sy|sz|tc|td|tel|tf|tg|th|tj|tk|tl|tm|tn|to|tp|travel|tr|tt|tv|tw|tz|ua|ug|uk|um|us|uy|uz|va|vc|ve|vg|vi|vn|vu|wf|ws|xn--0zwm56d|xn--11b5bs3a9aj6g|xn--80akhbyknj4f|xn--9t4b11yi5a|xn--deba0ad|xn--g6w251d|xn--hgbk6aj7f53bba|xn--hlcj6aya9esc7a|xn--jxalpdlp|xn--kgbechtv|xn--zckzah|ye|yt|yu|za|zm|zw)",
        HOST_OR_IP = "(?:" + HOSTNAME + TLD + "|" + IPV4 + ")",
        PATH = "(?:[;/][^#?<>\\s]*)?",
        QUERY_FRAG = "(?:\\?[^#<>\\s]*)?(?:#[^<>\\s]*)?",
        URI1 = "\\b" + SCHEME + "[^<>\\s]+",
        URI2 = "\\b" + HOST_OR_IP + PATH + QUERY_FRAG + "(?!\\w)",

        MAILTO = "mailto:",
        EMAIL = "(?:" + MAILTO + ")?[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@" + HOST_OR_IP + QUERY_FRAG + "(?!\\w)",

        URI_RE = new RegExp("^(?:" + URI1 + "|" + URI2 + "|" + EMAIL + ')$', "i"),
        SCHEME_RE = new RegExp("^" + SCHEME, "i"),

        quotes = {
          "'": "`",
          '>': '<',
          ')': '(',
          ']': '[',
          '}': '{',
          '»': '«',
          '›': '‹'
        };

      var el = $(element);
      var value = ko.utils.unwrapObservable(valueAccessor());
      var isLinkable = URI_RE.test(value);

      if (isLinkable) {
        var href = value;

        // Add appropriate protocol to naked links.
        if (!SCHEME_RE.test(href)) {
          href = (href.indexOf('@') !== -1 ? (!href.indexOf(MAILTO) ? '' : MAILTO)
            : !href.indexOf('irc.') ? 'irc://'
            : !href.indexOf('ftp.') ? 'ftp://'
            : 'http://') + href;
        }

        el.html('<a href="' + href + '" title="' + href + '" target="_blank">' + value + '</a>');
      }
      else {
        ko.bindingHandlers.text.update(element, valueAccessor);
      }
    }
  };

  // similar to original text binding but flashes background and color when value is updated
  // wrap original text binding to be able to check if value has actually changed
  ko.bindingHandlers.flashingText = {
    update: function (element, valueAccessor) {
      var value = ko.utils.unwrapObservable(valueAccessor());
      var prev = $(element).text();

      ko.bindingHandlers.text.update(element, valueAccessor);

      if (value && prev !== value.toString()) {
        $(element).stop(true, true);

        var color = $(element).css('color');
        var backgroundColor = $(element).css('background-color');

        $(element).css({
          backgroundColor: 'white',
          color: 'white'
        }).animate({
          backgroundColor: backgroundColor,
          color: color
        }, 500);
      }
    }
  };

  // simple pagination
  // observable currentPage and items
  ko.bindingHandlers.pagination = {
    init: function (element, valueAccessor, allBindingsAccessor) {
      var options = allBindingsAccessor().paginationOptions || {};
      if (ko.isObservable(options.items)) {
        var itemsObservable = options.items;
        itemsObservable.subscribe(function (newVal) {
          newVal = parseInt(newVal, 10);
          if (isNaN(newVal)) return;
          $(element).pagination('updateItems', newVal);
        });

        options.items = parseInt(ko.utils.unwrapObservable(itemsObservable), 10);
      }

      $.extend(options, {
        onPageClick: function (pageNumber, event) {
          var observable = valueAccessor();
          observable(pageNumber);
        }
      });

      $(element).pagination(options);

      ko.utils.domNodeDisposal.addDisposeCallback(element, function () {
        $(element).pagination('destroy');
      });
    },
    update: function (element, valueAccessor) {
      var value = parseInt(ko.utils.unwrapObservable(valueAccessor()), 10);
      if (isNaN(value)) value = 0;
      $(element).pagination('selectPage', value);
    }
  };

  // jQuery slider
  // observable max and value
  ko.bindingHandlers.slider = {
    init: function (element, valueAccessor, allBindingsAccessor) {
      var options = allBindingsAccessor().sliderOptions || {};
      if (ko.isObservable(options.max)) {
        var maxObservable = options.max;
        maxObservable.subscribe(function (newVal) {
          newVal = parseInt(newVal, 10);
          if (isNaN(newVal)) return;
          $(element).slider('option', 'max', newVal);
        });

        options.max = parseInt(ko.utils.unwrapObservable(maxObservable), 10);
      }
      $(element).slider(options);

      ko.utils.domNodeDisposal.addDisposeCallback(element, function () {
        $(element).slider('destroy');
      });
      ko.utils.registerEventHandler(element, 'slidechange', function (event, ui) {
        var observable = valueAccessor();
        observable(ui.value);
      });
      ko.utils.registerEventHandler(element, 'slide', function (event, ui) {
        var observable = valueAccessor();
        observable(ui.value);
      });
    },
    update: function (element, valueAccessor) {
      var value = parseInt(ko.utils.unwrapObservable(valueAccessor()), 10);
      if (isNaN(value)) value = 0;
      $(element).slider('value', value);
    }
  };

  // jQuery datepicker
  // observable timestamp
  ko.bindingHandlers.datepicker = {
    init: function (element, valueAccessor, allBindingsAccessor) {
      //initialize datepicker with some optional options
      var value = ko.utils.unwrapObservable(valueAccessor());
      var options = allBindingsAccessor().datepickerOptions || {};
      $.extend(options, {
        'dateFormat': 'm/d/yy'
      });
      $(element).datepicker(options);

      //handle the field changing
      ko.utils.registerEventHandler(element, 'change', function () {
        var observable = valueAccessor();
        var date = $(element).datepicker('getDate');

        if (options.endOfDay) {
          date.setHours(23);
          date.setMinutes(59);
          date.setSeconds(59);
          date.setMilliseconds(999);
        }

        var value = date.getTime();
        observable(value);
      });

      //handle disposal (if KO removes by the template binding)
      ko.utils.domNodeDisposal.addDisposeCallback(element, function () {
        $(element).datepicker('destroy');
      });
    },
    update: function (element, valueAccessor) {
      var value = ko.utils.unwrapObservable(valueAccessor());
      var current = $(element).datepicker('getDate');
      var dateStr = $.datepicker.formatDate('m/d/yy', new Date(value));
      var date = new Date(dateStr);

      if (date - current !== 0) {
        $(element).datepicker('setDate', date);
      }
    }
  };


  /* ===========================
   * OUC UTILS JQUERY EXTENSIONS
   * =========================== */
  var $banner = $('#banner');
  $banner.find('a.close').on('click', function () {
    $banner.slideUp();
  });

  $.oucUtils = {
    preload: function (imageSrcs) {
      var images = [];
      for (var i = 0; i < imageSrcs.length; i++) {
        images[i] = new Image();
        images[i].src = imageSrcs[i];
      }

      return images;
    },
    loadCss: function (href) {
      if (typeof LESSCSS !== 'undefined') {
        if (LESSCSS) return LESSCSS;
      }

      if ($('link[href="' + href + '"]').length > 0) return;

      return $('<link />').attr('type', 'text/css').attr('rel', 'stylesheet').attr('href', href).appendTo('head');
    },
    capitalize: function (string) {
      return string.charAt(0).toUpperCase() + string.slice(1);
    },
    notifyTimeout: 0,
    notify: function (text, level, timeout) {
      if (!text) return;
      if (!timeout) timeout = 5000;

      var css = 'alert-info';
      if (_.contains(['warn', 'error', 'success'], level)) {
        css = 'alert-' + level;
      }

      $banner.find('.alert').removeClass('alert-info alert-warn alert-error alert-success').addClass(css);
      $banner.find('p').html(text);
      $banner.slideDown();
      clearTimeout($.oucUtils.notifyTimeout);
      $.oucUtils.notifyTimeout = setTimeout(function () {
        $banner.slideUp();
      }, timeout);
    },
    blinkInterval: 0,
    blinkAlternateText: document.title,
    blinkTitleBar: function (text) {
      $.oucUtils.blinkAlternateText = document.title; // in case this has been changed
      $.oucUtils.blinkInterval = setInterval(function () {
        if (document.title === $.oucUtils.blinkAlternateText) {
          document.title = text;
        }
        else {
          document.title = $.oucUtils.blinkAlternateText;
        }
      }, 1000);
    },
    restoreTitleBar: function () {
      try {
        clearInterval($.oucUtils.blinkInterval);
        document.title = $.oucUtils.blinkAlternateText;
      }
      catch (e) {}
    },
    requestNotificationPermission: function () {
      if (!('Notification' in window)) {
        console.log('This browser does not support desktop notification');
        return;
      }

      if (Notification.permission === 'default') {
        notify('Click <a onclick="Notification.requestPermission();" href="#">here</a> if you want Reach to show notifications on your desktop.', 'info', 45000);
      }
    },
    alertDesktop: function (title, body) {
      if (!('Notification' in window)) {
        console.log('This browser does not support desktop notification');
        return;
      }

      if (Notification.permission === 'granted') {
        var notification = new Notification(title, {
          body: body,
          dir: 'auto',
          lang: '',
          tag: '',
          icon: 'images/ezuce-logo.png'
        });

        return notification;
      }
    },
    storeLocally: function (key, value) {
      if (!('localStorage' in window)) {
        console.log('This browser does not support local storage');
        return;
      }

      try {
        var parent = localStorage.getItem('OUCX');
        var parentObj = {};

        if (parent) {
          parentObj = JSON.parse(parent);
        }

        parentObj[key] = value;
        parent = JSON.stringify(parentObj);

        localStorage.setItem('OUCX', parent);
      }
      catch (e) {
        console.log(e);
      }
    },
    getValueFromLocal: function (key) {
      if (!('localStorage' in window)) {
        console.log('This browser does not support local storage');
        return;
      }

      try {
        var parent = localStorage.getItem('OUCX');
        var parentObj = {};

        if (parent) {
          parentObj = JSON.parse(parent);
          return parentObj[key];
        }

        return undefined;
      }
      catch (e) {
        console.log(e);
      }
    },
    storeInSession: function (key, value) {
      if (!('sessionStorage' in window)) {
        console.log('This browser does not support local storage');
        return;
      }

      try {
        var parent = sessionStorage.getItem('OUCX');
        var parentObj = {};

        if (parent) {
          parentObj = JSON.parse(parent);
        }

        parentObj[key] = value;
        parent = JSON.stringify(parentObj);

        sessionStorage.setItem('OUCX', parent);
      }
      catch (e) {
        console.log(e);
      }
    },
    getValueFromSession: function (key) {
      if (!('sessionStorage' in window)) {
        console.log('This browser does not support local storage');
        return;
      }

      try {
        var parent = sessionStorage.getItem('OUCX');
        var parentObj = {};

        if (parent) {
          parentObj = JSON.parse(parent);
          return parentObj[key];
        }

        return undefined;
      }
      catch (e) {
        console.log(e);
      }
    },
    clearSessionValues: function () {
      if (!('sessionStorage' in window)) {
        console.log('This browser does not support local storage');
        return;
      }

      sessionStorage.removeItem('OUCX');
    },
    removeSessionCookie: function () {
      $.removeCookie('OUCX', { path: '/' });
    }
  };
});
