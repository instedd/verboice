$(function() {
  // This is to expand shortened text in tables
  $("td").live('click', function() {
    spans = $(this).children("span");
    if (spans.length > 0) {
      $span = $(spans[0]);
      if ($span && $span.attr('title')) {
        $span.text($span.attr('title'));
        $span.attr('title', '');
      }
    }
  });
});

function create_channel(select) {
  if (!select.value) return;
  window.location = '/channels/new?kind=' + select.value;
  select.value = '';
}
