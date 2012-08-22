#= require resources/localized_resource

onResources(function(){
  window['UploadLocalizedResource']= function UploadLocalizedResource(hash, resource){
    LocalizedResource.call( this, hash, resource );

    this.label = 'Upload a file';
    this.template = 'upload_localized_resource_template';

    this.description = ko.observable(hash.description);
    this.hasAudio = ko.observable(hash.has_uploaded_audio);
    this.filename = ko.observable(hash.filename);

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
  }

  UploadLocalizedResource.prototype = new LocalizedResource();
  UploadLocalizedResource.prototype.constructor = UploadLocalizedResource;

  UploadLocalizedResource.prototype.toHash= function(){
    return $.extend(LocalizedResource.prototype.toHash.call( this ), { description: this.description(), filename: this.filename() })
  };
  UploadLocalizedResource.prototype.type= function(){
    return 'UploadLocalizedResource'
  }

  UploadLocalizedResource.prototype.download= function(){
    return downloadURL("/projects/" + project_id + "/resources/" + this.parent().id() + "/localized_resources/" + this.id() + "/play_file");
  }

  // fileupload callbacks
  UploadLocalizedResource.prototype.add= function(e, data){
    this.filename(data.files[0].name);
    return data.url = this.url();
  }

  UploadLocalizedResource.prototype.submit= function(){
    if (!isSaved()) {
      alert('Please save this message before uploading a file');
      return false;
    }
  }

  UploadLocalizedResource.prototype.done= function(){
    return this.hasAudio(true);
  }

  UploadLocalizedResource.prototype.preserveCurrentValues= function() {
    this.original_description = this.description();
  }

  UploadLocalizedResource.prototype.revertToPreservedValues= function() {
    this.description(this.original_description);
  }
})

