#= require resources/localized_resource
#= require recorder

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
    Wami.setup ({
      id: 'wami',
      swfUrl: '/Wami.swf',
      onReady: function(){
        Wami.startRecording(
          self.saveRecordingUrl(),
          Wami.nameCallback(function(){self.recording(true)}), // record started
          Wami.nameCallback(function(){self.recording(false)}), // record finished
          Wami.nameCallback(function(){self.recording(false)}) // record failed
        );
        self.recordingStart = self.nowSeconds();
        self.updateDurationInterval = window.setInterval( function(){ return self.updateDuration(self.nowSeconds() - self.recordingStart)}, 100 );
      }
    });
    this.alertFlashRequired('recording');
  }

  RecordLocalizedResource.prototype.stop= function(){
    // check if Wami is loaded
    if (Wami.stopRecording) {
      if (this.recording()) Wami.stopRecording();
      if (this.playing()) Wami.stopPlaying();
      this.hasAudio(true);
    }
    this.playing(false);
    window.clearInterval(this.updateDurationInterval);
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
    Wami.setup ({
      id: 'wami',
      swfUrl: '/Wami.swf',
      onReady: function(){
        window['playFinished'] = function(){ self.playing(false) };
        Wami.startPlaying(self.playRecordingUrl(), null, Wami.nameCallback(window['playFinished']));
      }
    })
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

})


