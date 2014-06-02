onResourcesWorkflow(function(){
  window['LocalizedResource']= function LocalizedResource(hash, resource){
    if (hash) {
      this.id = ko.observable(hash.id);
      this.language = ko.observable(hash['language'])
    }
    this.parent = ko.observable(resource);
    this.uploadStatus = ko.observable('standBy');
  }

  LocalizedResource.prototype.set_parent = function(parent) {
    return this.parent(parent);
  }

  LocalizedResource.prototype.beforeSave = function(){
  }

  LocalizedResource.prototype.afterSave = function(){
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
