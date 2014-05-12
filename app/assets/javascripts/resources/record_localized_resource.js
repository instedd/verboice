#= require resources/localized_resource

onResources(function(){
  window['RecordLocalizedResource']= function RecordLocalizedResource(hash, resource){
    LocalizedResource.call( this, hash, resource );

    this.label = 'Record message';
    this.template = 'record_localized_resource_template';

    this.hasAudio = ko.observable(hash.has_recorded_audio);
    this.recording = ko.observable(false);
    this.playing = ko.observable(false);
    this.duration = ko.observable(hash.duration || (new Date).clearTime().toString('mm:ss'));
    this.recordingStart = null;
    this.updateDurationInterval = null;
    this.description = ko.observable(hash.description);

    this.isValid = ko.computed(function(){
      this.hasAudio();
    }, this)

    var swfVersion = "11.7.0";
    var flashVars = {
      gain:50,
      rate:44.1,
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

  RecordLocalizedResource.prototype = new LocalizedResource();
  RecordLocalizedResource.prototype.constructor = RecordLocalizedResource;

  RecordLocalizedResource.prototype.record= function(){
    var self = this;
    if (this.playing() || this.recording()) return;
    if (!this.isSaved()) {
      alert('Please save this message before recording');
      return;
    }


    this.playing(false);
    this.updateDuration(0);

    var recorderElement = document.getElementById("recorder");
    recorderElement.record();

    // :(
    window.currentResource = self;

    recorderElement.addEventListener("recorderStart", "RecordLocalizedResource.prototype.startHandler");
    // recorderElement.addEventListener("recorderProgress", "RecordLocalizedResource.prototype.progressHandler");
    recorderElement.addEventListener("recorderComplete", "RecordLocalizedResource.prototype.completeHandler");
    // recorderElement.addEventListener("uploadError", "uploadErrorHandler");
    // recorderElement.addEventListener("uploadComplete", "uploadCompleteHandler");
    recorderElement.addEventListener("playbackStart", "RecordLocalizedResource.prototype.playbackStartHandler");
    recorderElement.addEventListener("playbackComplete", "RecordLocalizedResource.prototype.playbackCompleteHandler");
    // recorderElement.addEventListener("accessDenied", "accessDeniedHandler");
    // recorderElement.addEventListener("accessGranted", "accessGrantedHandler");
    // recorderElement.addEventListener("encoderError", "encoderErrorHandler");

    this.alertFlashRequired('recording');
  }

  // function uploadErrorHandler(info) {
  //   console.log("Error " + info.errorId + " " + info.text);
  // }

  // function uploadProgressHandler(info) {
  //   console.log("Progress" + info.bytesLoaded + "/" + info.bytesTotal);
  // }

  // function uploadCompleteHandler(info) {
  //   console.log("Upload Complete")
  //   console.log(info.data);
  // }

  RecordLocalizedResource.prototype.startHandler = function(info) {
    window.currentResource.recording(true);
    window.currentResource.recordingStart = window.currentResource.nowSeconds();
    window.timeHandler = window.setInterval( function(){ return window.currentResource.updateDuration(window.currentResource.nowSeconds() - window.currentResource.recordingStart)}, 1000 );
  }

  // function progressHandler(info) {
  //   console.log("Recorder Progress");
  // }

  RecordLocalizedResource.prototype.completeHandler = function(info) {
    clearInterval(window.timeHandler);
    window.currentResource.recording(false);
  }

  RecordLocalizedResource.prototype.playbackStartHandler = function(info) {
    window.currentResource.playbackStart1 = window.currentResource.nowSeconds();
    window.currentResource.updateDurationInterval = window.setInterval( function(){ return window.currentResource.updateDuration(window.currentResource.nowSeconds() - window.currentResource.playbackStart1)}, 100 );
  }

  RecordLocalizedResource.prototype.playbackCompleteHandler = function(info) {
    window.currentResource.playing(false);
    clearInterval(window.currentResource.updateDurationInterval);
  }

  // function accessDeniedHandler(info) {
  //   console.log("Access Denied");
  // }

  // function accessGrantedHandler(info) {
  //   console.log("Access Granted");
  // }

  // function encoderErrorHandler(info) {
  //   console.log("Encoder Error");
  // }

  // function playRaw2() {
  //   document.getElementById("recorder").playRaw("http://verboice-stg.instedd.org/projects/541/resources/8775/localized_resources/8884/play_recording");
  // }

  // function playMp3() {
  //   document.getElementById("recorder").playMp3("http://verboice-stg.instedd.org/projects/541/resources/8775/localized_resources/8884/play_recording");
  // }

  // function enabled() {
  //   console.log(document.getElementById("recorder").enabled());
  // }

  // function uploadRaw() {
  //   document.getElementById("recorder").uploadRaw(self.saveRecordingUrl());
  // }

  // function uploadMp3() {
  //   document.getElementById("recorder").uploadMp3("http://verboice-stg.instedd.org/projects/541/resources/8775/localized_resources/8884/save_recording");
  // }

  // function gain() {
  //   document.getElementById("recorder").gain(200);
  // }

  // function rate() {
  //   document.getElementById("recorder").rate(11);
  // }

  // function silentLevel() {
  //   document.getElementById("recorder").silentLevel(10);
  // }

  // function timeOut() {
  //   document.getElementById("recorder").timeOut(100);
  // }

  RecordLocalizedResource.prototype.stop= function(){
    document.getElementById("recorder").stop();
    window.currentResource.playing(false);
    clearInterval(window.currentResource.updateDurationInterval);
  }

  RecordLocalizedResource.prototype.toHash= function(){
    return $.extend(LocalizedResource.prototype.toHash.call( this ), { description: this.description(), duration: this.duration() });
  }

  RecordLocalizedResource.prototype.type= function(){
    return 'RecordLocalizedResource';
  }

  RecordLocalizedResource.prototype.play= function(){
    var self = this;
    if (this.playing() || this.recording() || !this.hasAudio()) return;
    this.recording(false);
    this.playing(true);
    document.getElementById("recorder").playRaw();
    this.alertFlashRequired('playing');
  }

  RecordLocalizedResource.prototype.saveRecordingUrl= function(){
    return "/projects/" + project_id + "/resources/" + this.parent().id() + "/localized_resources/" + this.id() + "/save_recording";
  }

  RecordLocalizedResource.prototype.playRecordingUrl= function(){
    return "/projects/" + project_id + "/resources/" + this.parent().id() + "/localized_resources/" + this.id() + "/play_recording";
  }

  RecordLocalizedResource.prototype.nowSeconds= function(){
    return Math.round(new Date()/1000);
  }

  RecordLocalizedResource.prototype.updateDuration= function(seconds){
    return this.duration((new Date).clearTime().addSeconds(seconds).toString('mm:ss'));
  }

  RecordLocalizedResource.prototype.alertFlashRequired= function(){
    if ($('.flash-required').length) {
      $('.flash-required').html('');
      alert("Adobe Flash Player version 10.0.0 or higher is required for " + action + " a message.\nDownload it from https://get.adobe.com/flashplayer/ and reload this page.");
    }
  }

  RecordLocalizedResource.prototype.preserveCurrentValues= function() {
    this.original_description = this.description();
  }

  RecordLocalizedResource.prototype.revertToPreservedValues= function() {
    this.description(this.original_description);
  }
})


