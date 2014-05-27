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
      self.hasAudio(true);
      self.filename(data.files[0].name);
      self.uploadedfile = data;
    }

    this.done= function(){
      self.hasAudio(true);
    }

    this.showProgress = function (e, data) {
      // Log the current bitrate for this upload:
      // debugger
      console.log(data);
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
    return downloadURL("/projects/" + project_id + "/resources/" + this.parent().id() + "/localized_resources/" + this.id() + "/play_file");
  }

  // fileupload callbacks
  // UploadLocalizedResource.prototype.add= function(e, data){
  //   this.filename(data.files[0].name);
  //   return data.url = this.url();
  // }

  // UploadLocalizedResource.prototype.submit= function(){
  //   if (!this.isSaved()) {
  //     alert('Please save this message before uploading a file');
  //     return false;
  //   }
  // }

  // UploadLocalizedResource.prototype.done= function(){
  //   return this.hasAudio(true);
  // }

  UploadLocalizedResource.prototype.preserveCurrentValues= function() {
    this.original_description = this.description();
  }

  UploadLocalizedResource.prototype.revertToPreservedValues= function() {
    this.description(this.original_description);
  }

  UploadLocalizedResource.prototype.afterSave = function(){
    //In order to trigger the update in the jqueryfile upload binding
    console.log("en afrer save");
    console.log(this.url());
    this.uploadedfile.url = this.url();
    this.uploadedfile.submit();
  }
})

