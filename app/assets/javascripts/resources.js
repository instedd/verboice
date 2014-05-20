#= require_tree ./resources

onResources(function(){
  function loadRecorderSwf() {
    var swfVersion = "11.7.0";
    var flashVars = {
      gain:50,
      silentLevel:0,
      timeOut:-1
    }

    var params = {
      menu: "false",
      quality: "high",
      allowscriptaccess: "always",
      allowfullscreen: "true",
      wmode: "opaque"
    }

    var attributes = {
      id: "recorder",
      name: "recorder"
    }
    swfobject.embedSWF("/Recorder.swf", "recorder", "220", "150", swfVersion, null, flashVars, params, attributes);
  }

  window.project = new Project();
  ko.applyBindings(project);
  loadRecorderSwf();
  window.onbeforeunload = function() {
    editing_any = _.some(project.resources(), function(res) { return res.editing() });
    if (editing_any) {
      return "If you leave this page without saving the resources you are editing your changes will be lost.";
    };
    return null;
  };

})
