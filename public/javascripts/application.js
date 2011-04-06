function create_channel(select) {
  if (!select.value) return;
  window.location = '/channels/new?kind=' + select.value;
  select.value = '';
}
