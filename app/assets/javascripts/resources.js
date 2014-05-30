#= require_tree ./resources

onResources(function(){
  loadRecorderSwf();
  window.project = new Project();
  ko.applyBindings(project, document.getElementById('container'));
  window.onbeforeunload = function() {
    editing_any = _.some(project.resources(), function(res) { return res.editing() });
    if (editing_any) {
      return "If you leave this page without saving the resources you are editing your changes will be lost.";
    };
    return null;
  };

})

onWorkflow(function(){
  loadRecorderSwf();
})
