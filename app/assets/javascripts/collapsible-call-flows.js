$(function() {
  // Support collapsible items by running handlers and applying classes
  $('.call_flow_box > span:first-child > a, .call_flow_box div .collapse_trigger').live('click', function(){
    var collapsible = $(this).closest('.call_flow_box');
    var trigger = $(this);

    collapsible.toggleClass('collapsed');
    collapsible.toggleClass('expanded');
    var collapsed = collapsible.hasClass('collapsed');

    var applyClasses = function(item) {
      var onExpanded = item.data('on-expanded');
      var onCollapsed = item.data('on-collapsed');
      if (onExpanded) item.toggleClass(onExpanded, !collapsed);
      if (onCollapsed) item.toggleClass(onCollapsed, collapsed);
    };

    var runHandlers = function(item) {
      var handler = item.data('collapse-handler');
      if (handler) window[handler].call(item, collapsed);
    }

    applyClasses(collapsible);
    applyClasses(trigger);

    runHandlers(collapsible);
    runHandlers(trigger);

    return false;
  });

  $('.call_flow_mode select').live('change', function(){
    var call_flow = $(this).closest('.call_flow_box');
    call_flow.toggleClass('flow_mode');
    call_flow.toggleClass('callback_mode');
  });

  $('.remove_unsaved_call_flow').each(function(){
    $(this).live('click', function(){
      $(this).closest(".call_flow_box").remove()
    });
  });

  $('.call_flow_box form').live('ajax:complete', function(triggered_event, xml_http_request, ajax_options){
    var call_flow_box = $(this).closest('.call_flow_box_container');
    call_flow_box.html(xml_http_request.responseText);
  });
});

function add_fields(link, content) {
  $(link).closest(".call_flow_box").before(content);
}


