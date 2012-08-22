#= require_tree ./resources

onResources(function(){
  window.project = new Project();
  ko.applyBindings(project);
})
