#= require resources/localized_resource

onResourcesWorkflow(function(){
  window['UploadLocalizedResource']= function UploadLocalizedResource(hash, resource){
    LocalizedResource.call( this, hash, resource );
    var self = this;
    this.label = 'Upload a file';
    this.template = 'upload_localized_resource_template';

    this.description = ko.observable(hash.description);
    this.hasAudio = ko.observable(hash.has_uploaded_audio);
    this.filename = ko.observable(hash.filename);
    this.uploadedfile = null;


    this.url = ko.computed(function(){
      if(this.isSaved()) {
        return "/projects/" + project_id + "/resources/" + this.parent().id() + "/localized_resources/" + this.id() + "/save_file?filename=" + this.filename()
      } else {
        return null
      }
    }, this);

    this.isValid = ko.computed(function(){
      return this.hasAudio();
    }, this)

    // fileupload callbacks
    this.add= function(e, data){
      self.preserveCurrentValues();
      self.hasAudio(true);
      self.filename(data.files[0].name);
      self.uploadStatus('pending');
      self.uploadedfile = data;
    }

    this.showProgress = function (e, data) {
      var progress = parseInt(data.loaded / data.total * 100, 10);
      self.parent().uploadProgress(progress);
    }

    this.fail = function (e, data) {
      self.uploadStatus('error');
      self.revertToPreservedValues();
    }

    this.done = function(){
      self.uploadStatus('ok');
    }
  }

  UploadLocalizedResource.prototype = new LocalizedResource();
  UploadLocalizedResource.prototype.constructor = UploadLocalizedResource;

  UploadLocalizedResource.prototype.toHash= function(){
    return $.extend(LocalizedResource.prototype.toHash.call( this ), { description: this.description(), filename: this.filename()})
  };
  UploadLocalizedResource.prototype.type= function(){
    return 'UploadLocalizedResource'
  }

  UploadLocalizedResource.prototype.download= function(){
    if (!this.parent().id() || !this.id()) {
      return false;
    }

    return downloadURL("/projects/" + project_id + "/resources/" + this.parent().id() + "/localized_resources/" + this.id() + "/play_file");
  }

  UploadLocalizedResource.prototype.preserveCurrentValues= function() {
    this.original_description = this.description();
    this.original_hasAudio = this.hasAudio();
    this.original_filename = this.filename();
  }

  UploadLocalizedResource.prototype.revertToPreservedValues= function() {
    this.description(this.original_description);
    this.hasAudio(this.original_hasAudio);
    this.filename(this.original_filename);
  }

  UploadLocalizedResource.prototype.afterSave = function(){
    if (this.uploadedfile) {
      this.uploadStatus('uploading');
      this.uploadedfile.url = this.url();
      // binding the events in the view and in knockout bindings is not working for some reason :(
      this.uploadedfile.submit().done(this.done).fail(this.fail);
    } else {
      this.uploadStatus('ok');
    }
  }
})

