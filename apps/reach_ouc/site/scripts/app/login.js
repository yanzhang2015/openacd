define([
  'viewModels/header'
], function (header) {
  //resizes container-login div onload and on window resize
  function resize() {
    var height = $(window).height() - 300;
    $('#container-login').height(height);
  }

  function login(action, username, password, sip_uri, remember, force) {
    var params = {};

    if(username) {
      params.username = username;
    }

    if(password) {
        params.password = password;
    }

    if(sip_uri) {
      params.sip_uri = sip_uri;
    }

    if(remember) {
        params.remember = remember;
    }

    if(force) {
      params.force = force;
    }

    var result = $.post(action, params, function (data, textStatus, jqXHR) {
      $.oucUtils.storeInSession('token', data.token);
      window.location = data.redirect_location;
    }, 'json');
    result.fail(function( jqXHR, textStatus, errorThrown){
      if(errorThrown == "Unauthorized") {
        errorThrown = "Invalid username or password.";
      } else if(errorThrown == "Forbidden") {
        errorThrown = "The number of concurrent users exceeded license limit.";
      }
      else {
        errorThrown = "The system is currently unavailable. Please try again later.";
      }
      $('#errorLogin').html(errorThrown);
      $('#errorLogin').show();
    });
  }

  function toggleAdvancedOptions() {
    $('#advanced-options-title i').toggleClass('icon-arrow-right icon-arrow-down');
    $('#container-login .well.login').toggleClass('advanced', 400);
    $('#advanced-options').slideToggle();
  }

  $(function () {
    resize();
    $(window).resize(function () {
      resize();
    });

    $('#login-form').submit(function (event) {
      event.preventDefault();
      login($(this).attr('action'), $('#username').val(), $('#password').val(), false,  $('#remember').val(), $('#force').val());
    });

    $('#advanced-form').submit(function (event) {
      event.preventDefault();
      login($(this).attr('action'), false, false, $('#sip_uri').val(), false, $('#force').val());
    });

    $('#advanced-options-title span').click(toggleAdvancedOptions);
  });
});
