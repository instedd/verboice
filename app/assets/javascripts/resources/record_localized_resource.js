#= require resources/localized_resource

onResourcesWorkflow(function(){
  window['RecordLocalizedResource']= function RecordLocalizedResource(hash, resource){
    LocalizedResource.call( this, hash, resource );

    this.label = 'Record message';
    this.template = 'record_localized_resource_template';

    this.hasAudio = ko.observable(hash.has_recorded_audio);
    this.recording = ko.observable(false);
    this.setGlobalRecording(false);
    this.playing = ko.observable(false);
    this.setGlobalPlaying(false);
    this.duration = ko.observable(hash.duration || (new Date).clearTime().toString('mm:ss'));
    this.description = ko.observable(hash.description);

    this.recordingStart = null;
    this.updateDurationInterval = null;
    this.totalDuration = this.duration();
    this.listenersInitialized = false;
    this.guid = Math.floor((Math.random() * 1000) + 1);

    this.isValid = ko.computed(function(){
      return this.hasAudio();
    }, this)
  }

  RecordLocalizedResource.prototype = new LocalizedResource();
  RecordLocalizedResource.prototype.constructor = RecordLocalizedResource;

  RecordLocalizedResource.prototype.globalRecording = function(){
    return window.localizedResourceRecording();
  }

  RecordLocalizedResource.prototype.setGlobalRecording = function(value){
    if(window.localizedResourceRecording == undefined){
      window.localizedResourceRecording = ko.observable(value);
    } else {
      window.localizedResourceRecording(value);
    }
    this.recording(value);
  }

  RecordLocalizedResource.prototype.globalPlaying = function(){
    return window.localizedResourcePlaying();
  }

  RecordLocalizedResource.prototype.setGlobalPlaying = function(value){
    if(window.localizedResourcePlaying == undefined){
      window.localizedResourcePlaying = ko.observable(value);
    } else {
      window.localizedResourcePlaying(value);
    }
    this.playing(value);
  }

  RecordLocalizedResource.prototype.record = function(){
    if (this.playing() || this.recording()) return;

    var recorderElement = document.getElementById("recorder");
    this.initListeners(this, recorderElement);

    recorderElement.record(this.guid);

    this.alertFlashRequired('recording');
  }

  RecordLocalizedResource.prototype.initListeners = function(currentResource, recorderElement){
    window.currentResource = currentResource;
    if(!this.listenersInitialized){
      recorderElement.addEventListener("recorderStart", "RecordLocalizedResource.prototype.startHandler");
      recorderElement.addEventListener("recorderComplete", "RecordLocalizedResource.prototype.completeHandler");
      recorderElement.addEventListener("playbackComplete", "RecordLocalizedResource.prototype.playbackCompleteHandler");
      recorderElement.addEventListener("panelOpened", "RecordLocalizedResource.prototype.showFlashPanel");
      recorderElement.addEventListener("panelClosed", "RecordLocalizedResource.prototype.hideFlashPanel");

      this.listenersInitialized = true;
    }
  }

  RecordLocalizedResource.prototype.startHandler = function(info) {
    window.currentResource.setGlobalPlaying(false);
    window.currentResource.updateDuration(0);
    window.currentResource.setGlobalRecording(true);
    window.currentResource.recordingStart = window.currentResource.nowSeconds();
    window.timeHandler = window.setInterval( function(){ return window.currentResource.updateDuration(window.currentResource.nowSeconds() - window.currentResource.recordingStart)}, 500 );
  }

  RecordLocalizedResource.prototype.completeHandler = function(info) {
    clearInterval(window.timeHandler);
    window.currentResource.totalDuration = window.currentResource.convertSecondsToString(window.currentResource.nowSeconds() - window.currentResource.recordingStart);
    window.currentResource.setGlobalRecording(false);
    window.currentResource.hasAudio(true);
    window.currentResource.uploadStatus('pending');
  }

  RecordLocalizedResource.prototype.playbackCompleteHandler = function(info) {
    window.currentResource.setGlobalPlaying(false);
    clearInterval(window.currentResource.updateDurationInterval);
    window.currentResource.duration(window.currentResource.totalDuration);
  }

  RecordLocalizedResource.prototype.hideFlashPanel = function() {
    $(recorder).removeClass("recorder-visible");
  }

  RecordLocalizedResource.prototype.showFlashPanel = function() {
    $(recorder).addClass("recorder-visible");
  }

  RecordLocalizedResource.prototype.stop= function(){
    document.getElementById("recorder").stop();
    this.setGlobalPlaying(false);
    clearInterval(this.updateDurationInterval);
    this.duration(this.totalDuration);
  }

  RecordLocalizedResource.prototype.toHash= function(){
    var recorder = document.getElementById("recorder");
    var resourceData = { description: this.description(), duration: this.duration()};
    if (recorder.hasData(this.guid)) {
      resourceData["encoded_audio"] = recorder.data(this.guid);
    }
    return $.extend(LocalizedResource.prototype.toHash.call( this ), resourceData);
  }

  RecordLocalizedResource.prototype.type= function(){
    return 'RecordLocalizedResource';
  }

  RecordLocalizedResource.prototype.play= function(){
    var recorder = document.getElementById("recorder");
    this.initListeners(this, recorder);
    if (this.playing() || this.recording() || (!this.hasAudio() && !recorder.hasData(this.guid))) return;
    this.setGlobalRecording(false);
    this.setGlobalPlaying(true);
    var self = this;
    this.playbackStart = this.nowSeconds();
    this.updateDuration(0);
    this.updateDurationInterval = window.setInterval( function(){ return self.updateDuration(self.nowSeconds() - self.playbackStart)}, 500 );

    if (recorder.hasData(this.guid)) {
      recorder.play(this.guid);
    } else {
      if (this.isSaved()) {
        recorder.playExternal(this.playRecordingUrl());
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
    this.original_duration = this.duration();
  }

  RecordLocalizedResource.prototype.revertToPreservedValues= function() {
    this.description(this.original_description);
    var recorder = document.getElementById("recorder");
    this.duration(this.original_duration);
    this.totalDuration = this.duration();
    recorder.clear(this.guid);
  }

  RecordLocalizedResource.prototype.beforeSave = function(){
    if (this.playing() || this.recording()) this.stop();
    if (this.hasAudio()) this.uploadStatus('uploading');
  }

  RecordLocalizedResource.prototype.afterSave = function(){
    if (this.hasAudio()) this.uploadStatus('ok');
  }

  RecordLocalizedResource.prototype.afterSaveFailed = function(){
    if (this.hasAudio()) this.uploadStatus('error');
  }
})


