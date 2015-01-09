//= require ./contacts_filter

function initScheduledCalls() {
  $(function() {
    $('.scheduled_call').each(function(i, e) {
      initScheduledCall(e);
    });
  });
}

function initScheduledCall(container) {
  var filters = JSON.parse($('.filters', container).val());
  var model = new ContactsFilter(filters);
  ko.applyBindings(model, container);

  $('input[alt=time]', container).setMask();

  $(".datepicker", container)
    .datepicker({showButtonPanel: false, dateFormat: 'yy-mm-dd'})
    .addClass('ux-datepicker');

  $('.date-time-trigger', container).change(function() {
    var enabled = $(this).attr('checked');
    var parent = $(this).closest('.date-time');
    $('input[type=text]', parent).attr('readonly', !enabled);
    $('.datepicker', parent).datepicker(enabled ? 'enable' : 'disable');
  }).trigger('change');

  var time_range = $('.time-range', container);
  var from_time = $('.from', time_range);
  var to_time = $('.to', time_range);

  time_range.timeRange({
    slide: function(e, ui) {
      from_time.val(ui.values[0]);
      to_time.val(ui.values[1]);
    },
    values: [from_time.val(), to_time.val()]
  });
}

function add_scheduled_call_box(e, fields) {
  add_box(e, fields);
  var container = $('.scheduled_call').last().get(0);
  initScheduledCall(container);
}
