$(function() {
  $('.enqueue_new_call input:checkbox#not_before').live('change', function(){
    $('.enqueue_new_call #not_before_date').attr('readonly',! $(this).attr('checked'));
  });
  $('.enqueue_new_call #not_before_date').attr('readonly', true);
});