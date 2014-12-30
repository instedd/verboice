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

  $('.not_before_enabled', container).change(function(){
    var container = $(this).closest('.not_before')[0];
    var enabled = $(this).attr('checked');
    $('input[type=text]', container).attr('readonly', !enabled);
    $(".ux-datepicker", container ).datepicker(enabled ? 'enable' : 'disable');
  });

  $('.not_after_enabled', container).change(function(){
    var container = $(this).closest('.not_after')[0];
    var enabled = $(this).attr('checked');
    $('input[type=text]', container).attr('readonly', !enabled);
    $(".ux-datepicker", container ).datepicker(enabled ? 'enable' : 'disable');
  });

  $('input[alt=time]', container).setMask();

  $(".ux-datepicker", container).datepicker({showButtonPanel: false, dateFormat: 'yy-mm-dd'});
}

function add_scheduled_call_box(e, fields) {
  add_box(e, fields);
  var container = $('.scheduled_call').last().get(0);
  initScheduledCall(container);
}
