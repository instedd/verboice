onResourcesWorkflow(function(){
  window['LocalizedResource']= function LocalizedResource(hash, resource){
    var self = this;
    if (hash) {
      this.id = ko.observable(hash.id);
      this.language = ko.observable(hash['language'])
    }
    this.parent = ko.observable(resource);
    this.uploadStatus = ko.observable('standBy');

    // after 5 seconds the status should return to standBy in order to hide the 'upload ok' icon
    this.uploadStatus.subscribe(function(newValue) {
      if (newValue == 'ok') {
        setTimeout(function() {self.uploadStatus('standBy');}, '5000');
      }
    });

  }

  LocalizedResource.prototype.set_parent = function(parent) {
    return this.parent(parent);
  }

  LocalizedResource.prototype.beforeSave = function(){
    this.uploadStatus('uploading');
  }

  LocalizedResource.prototype.afterSave = function(){
    this.uploadStatus('ok');
  }

  LocalizedResource.prototype.afterSaveFailed = function(){
    this.uploadStatus('error');
  }

  LocalizedResource.prototype.isSaved = function(){
    return this.parent() && this.parent().id() && this.id()
  }

  LocalizedResource.prototype.toHash = function() {
    return {
      id: this.id(),
      language: this.language(),
      type: this.type()
    }
  }
})
