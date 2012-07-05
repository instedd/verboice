$(function() {
  $('.call_flow_mode select').live('change', function(){
    var call_flow = $(this).closest('.collapsible_crud_box');
    call_flow.toggleClass('flow_mode');
    call_flow.toggleClass('callback_mode');
  });

  $('.store_in_fusion_tables').live('change', function(){
    var call_flow = $(this).closest('.collapsible_crud_box');
    $('.fusion_tables_settings', call_flow).toggle($(this).is(':checked'));
  }).change();

  $('.link_to_oauth').live('click', function(){
    var self = $(this);
    var url = self.attr('href');
    var fusion_table_name = self.siblings('.fusion_table_name').val();
    if (fusion_table_name) {
      url = url + '?fusion_table_name=' + fusion_table_name;
    }
    window.location = url;
    return false;
  });

});