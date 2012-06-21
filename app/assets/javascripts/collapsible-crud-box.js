$(function() {
  // Support collapsible items by running handlers and applying classes
  $('.collapsible_crud_box > span:first-child > a, .collapsible_crud_box div .collapse_trigger').live('click', function(){
    var collapsible = $(this).closest('.collapsible_crud_box');
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

  $('.remove_unsaved_element').live('click', function(){
    $(this).closest(".collapsible_crud_box").remove()
  });

  $('.collapsible_crud_box form').live('ajax:complete', function(triggered_event, xml_http_request, ajax_options){
    var collapsible_crud_box = $(this).closest('.collapsible_crud_box_container');
    collapsible_crud_box.html(xml_http_request.responseText);
  });
});

function add_box(link, content) {
  $(link).closest(".addqueue").before(content);
}


