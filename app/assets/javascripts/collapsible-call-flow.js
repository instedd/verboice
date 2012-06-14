$(function() {
  $('.call_flow_mode select').live('change', function(){
    var call_flow = $(this).closest('.collapsible_crud_box');
    call_flow.toggleClass('flow_mode');
    call_flow.toggleClass('callback_mode');
  });
});