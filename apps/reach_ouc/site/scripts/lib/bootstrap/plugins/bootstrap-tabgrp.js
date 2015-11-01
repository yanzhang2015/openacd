/* ========================================================
 * based on bootstrap-tab.js v2.2.1
 * http://twitter.github.com/bootstrap/javascript.html#tabs
 * ======================================================== */
!function ($) {

  "use strict"; // jshint ;_;


 /* TABGRP CLASS DEFINITION
  * ==================== */

  var TabGrp = function (element) {
    this.element = $(element).find('a')
  }

  TabGrp.prototype = {

    constructor: TabGrp

  , show: function () {
      var $this = this.element
        , $ul = $this.closest('.tab-group-container')
        , selector = $this.attr('data-target')
        , previous
        , $target
        , e

      if (!selector) {
        selector = $this.attr('href')
        selector = selector && selector.replace(/.*(?=#[^\s]*$)/, '') //strip for ie7
      }

      if ( $this.parents('.tab-group').hasClass('active') ) return

      previous = $ul.find('.active:last a')[0]

      e = $.Event('show', {
        relatedTarget: previous
      })

      $this.trigger(e)

      if (e.isDefaultPrevented()) return

      $target = $(selector)

      this.activate($this.parents('.tab-group'), $ul)
      this.activate($target, $target.parent(), function () {
        $this.trigger({
          type: 'shown'
        , relatedTarget: previous
        })
      })
    }

  , activate: function (element, container, callback) {
      var $active = container.find('> .active')
        , transition = callback
            && $.support.transition
            && $active.hasClass('fade')

      function next() {
        $active
          .removeClass('active')
          .find('> .dropdown-menu > .active')
          .removeClass('active')

        element.addClass('active')

        if (transition) {
          element[0].offsetWidth // reflow for transition
          element.addClass('in')
        } else {
          element.removeClass('fade')
        }

        // if ( element.parent('.dropdown-menu') ) {
        //   element.closest('li.dropdown').addClass('active')
        // }

        callback && callback()
      }

      transition ?
        $active.one($.support.transition.end, next) :
        next()

      $active.removeClass('in')
    }
  }


 /* TABGRP PLUGIN DEFINITION
  * ===================== */

  $.fn.tabgrp = function ( option ) {
    return this.each(function () {
      var $this = $(this)
        , data = $this.data('tabgrp')
      if (!data) $this.data('tabgrp', (data = new TabGrp(this)))
      if (typeof option == 'string') data[option]()
    })
  }

  $.fn.tabgrp.Constructor = TabGrp


 /* TABGRP DATA-API
  * ============ */

  $(document).on('click.tabgrp.data-api', '[data-toggle="tabgrp"], [data-toggle="pill"]', function (e) {
    e.preventDefault()
    $(this).tabgrp('show')
  })

}(window.jQuery);