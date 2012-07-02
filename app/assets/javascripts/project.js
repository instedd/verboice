$(function() {
  $('.enqueue_new_call input:checkbox#not_before').live('change', function(){
    $('#not_before_date, #not_before_time').attr('readonly',! $(this).attr('checked'));
  });
  $('#not_before_date, #not_before_time').attr('readonly', true);
  $('#not_before_time').setMask();
});