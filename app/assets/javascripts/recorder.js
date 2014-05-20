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
    wmode: "transparent"
  }

  var attributes = {
    id: "recorder",
    name: "recorder"
  }
  swfobject.embedSWF("/Recorder.swf", "recorder", "220", "150", swfVersion, null, flashVars, params, attributes);
}
