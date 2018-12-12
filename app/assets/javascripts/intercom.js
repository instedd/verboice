function fixIntercomLocation() {
  var footerPos = $('#footer').offset()
  if (footerPos && typeof Intercom !== 'undefined') {
    var footerTop = footerPos.top - 20
    var bottom = $(window).scrollTop() + $(window).height();
    var padding = Math.max(0, bottom - footerTop)
    Intercom('boot', { vertical_padding: padding })
  }
}

var fixIntercomTimer = null;

function timedFixIntercomLocation() {
  if (fixIntercomTimer) {
    clearTimeout(fixIntercomTimer);
  }
  fixIntercomTimer = setTimeout(function() {
    fixIntercomTimer = null;
    fixIntercomLocation();
  }, 100);
}

$(window).scroll(timedFixIntercomLocation);
$(window).resize(timedFixIntercomLocation);
$(fixIntercomLocation);
