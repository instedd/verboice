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
    this.totalDuration = this.duration();

    this.isValid = ko.computed(function(){
      this.hasAudio();
    }, this)
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
    window.timeHandler = window.setInterval( function(){ return window.currentResource.updateDuration(window.currentResource.nowSeconds() - window.currentResource.recordingStart)}, 500 );
  }

  // function progressHandler(info) {
  //   console.log("Recorder Progress");
  // }

  RecordLocalizedResource.prototype.completeHandler = function(info) {
    clearInterval(window.timeHandler);
    window.currentResource.totalDuration = window.currentResource.convertSecondsToString(window.currentResource.nowSeconds() - window.currentResource.recordingStart);
    window.currentResource.recording(false);
    window.currentResource.hasAudio(true);
  }

  // RecordLocalizedResource.prototype.playbackStartHandler = function(info) {
  // }

  RecordLocalizedResource.prototype.playbackCompleteHandler = function(info) {
    window.currentResource.playing(false);
    clearInterval(window.currentResource.updateDurationInterval);
    window.currentResource.duration(window.currentResource.totalDuration);
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
    this.playing(false);
    clearInterval(this.updateDurationInterval);
    this.duration(this.totalDuration);
  }

  RecordLocalizedResource.prototype.toHash= function(){
    var recorder = document.getElementById("recorder");
    return $.extend(LocalizedResource.prototype.toHash.call( this ), { description: this.description(), duration: this.duration(), recorded_audio: recorder.data()});
  }

  RecordLocalizedResource.prototype.type= function(){
    return 'RecordLocalizedResource';
  }

  RecordLocalizedResource.prototype.play= function(){
    var recorder = document.getElementById("recorder");
    if (this.playing() || this.recording() || (!this.hasAudio() && !recorder.hasData())) return;
    this.recording(false);
    this.playing(true);
    var self = this;
    this.playbackStart = this.nowSeconds();
    this.updateDurationInterval = window.setInterval( function(){ return self.updateDuration(self.nowSeconds() - self.playbackStart)}, 500 );

    if (recorder.hasData()) {
      recorder.playRaw();
    } else {
      if (this.isSaved()) {
        recorder.playRaw(this.playRecordingUrl());
      }
    }
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
    return this.duration(this.convertSecondsToString(seconds));
  }

  RecordLocalizedResource.prototype.convertSecondsToString = function(seconds){
    return (new Date).clearTime().addSeconds(seconds).toString('mm:ss');
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


