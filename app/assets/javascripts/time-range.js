(function($) {

  $.fn.timeRange = function(options) {
    var settings = $.extend({
      values: [600, 900]
    }, options );

    var label = $('<div></div>').addClass('ux-time-range-label');
    var slider = $('<div></div>').addClass('ux-time-range-slider');
    var scale = $('<div></div>').addClass('ux-time-range-scale').css('position', 'relative');

    this.append(label).append(slider).append(scale);

    for(var i = 0; i < 13; i++) {
      var time = (i * 4 % 24) + ":00";
      time = time.length < 5 ? "0" + time : time;
      var offset = (Math.round(i / 12 * 10000) / 100) + '%';
      var e = $('<span></span>').html(time)
        .css('position', 'absolute')
        .css('left', offset);
      scale.append(e);
    }

    var formatTime = function(value) {
      var h = parseInt(value / 60 % 24, 10) + "";
      var m = parseInt(value % 60, 10) + "";
      h = h.length < 2 ? "0" + h : h;
      m = m.length < 2 ? "0" + m : m;
      return h + ":" + m;
    };

    var formatLabel = function(values) {
      var from = 'From ' + formatTime(values[0]);
      if (values[0] > 1440) {
        from = from + ' on next day';
      }
      var to = 'to ' + formatTime(values[1]);
      if (values[1] > 1440) {
        to = to + ' on next day';
      }
      label.html(from + ' ' + to);
    };

    var slide = function(e, ui) {
      formatLabel(ui.values);

      if (settings.slide) {
        options.slide(e, ui);
      }
    };

    slider.slider({
      range: true,
      min: 0,
      max: 2880,
      step: 15,
      slide: slide,
      values: settings.values
    });

    formatLabel(settings.values);

    return this;
  };

}(jQuery));
