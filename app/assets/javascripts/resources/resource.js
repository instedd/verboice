onResources(function(){
  window['Resource']= function Resource(hash, project){

    var self = this;
    this.id = ko.observable(null);
    this.guid = ko.observable(null);
    this.name = ko.observable(null);
    this.editing = ko.observable(false);

    var existing_localized_resources = hash['localized_resources'] || [];
    this.localizedResources = ko.observableArray(
      _.map(project.languages(), function(language){
        var localizedResource = _.detect(existing_localized_resources, function(resource){ return resource.language == language.iso()});
        localizedResource = localizedResource || { language: language.iso() };
        return LocalizedResourceSelector.fromHash(localizedResource, self);
      })
    );

    this.firstResource = ko.computed(function() {
      return _.detect(this.localizedResources(), function(res) { return project.firstLanguage().iso() == res.language() })
    }, this);
    this.secondResource = ko.computed(function() {
      return _.detect(this.localizedResources(), function(res) { return project.secondLanguage().iso() == res.language() })
    }, this);
  }

  Resource.fromHash = function(hash, project){
    var resource = new Resource(hash, project);

    resource.id(hash['id']);
    resource.guid(hash['guid']);
    resource.name(hash['name']);

    return resource;
  }

  Resource.prototype.edit = function(){
    if (this.editing()) {
      return true;
    } else {
      this.editing(true);
      this.preserveCurrentValues();
    }
  }

  Resource.prototype.save = function(){
    var self = this;
    var data = this.toHash();
    if(this.id()) {
      data._method = 'put'
      $.post("/projects/" + project_id + "/resources/" + this.id() + ".json", data, function(response){
        self.updateLocalizedResources(response.localized_resources);
      })
    } else {
      $.post("/projects/" + project_id + "/resources.json", data, function(response){
        self.id(response.id);
        self.guid(response.guid);
        self.updateLocalizedResources(response.localized_resources);
      })
    };
    this.editing(false);
  }

  Resource.prototype.cancel = function(){
    this.editing(false);
    this.revertToPreservedValues();
  }

  Resource.prototype.preserveCurrentValues= function() {
    this.original_name = this.name();
    _.each(this.localizedResources(), function(localized) {localized.preserveCurrentValues()})
  }

  Resource.prototype.revertToPreservedValues= function() {
    this.name(this.original_name);
    _.each(this.localizedResources(), function(localized) {localized.revertToPreservedValues()})
  }

  Resource.prototype.toHash= function(){
    if (this.id()) {
      return {
        id: this.id(),
        guid: this.guid(),
        project_id: this.project_id,
        resource: {
          name: this.name(),
          localized_resources_attributes: this.packLocalizedResources()
        }
      }
    } else {
      return {
        project_id: this.project_id,
        resource: {
          name: this.name(),
          localized_resources_attributes: this.packLocalizedResources()
        }
      }
    }
  }

  Resource.prototype.packLocalizedResources= function(){
    var result = {}
    _.each(this.localizedResources(), function(lr, i) {
      result[i] = lr.toHash();
    })
    return result;
  }

  Resource.prototype.updateLocalizedResources = function(arr) {
    _.each(arr, function(hash) {
        localizedResource = _.detect(this.localizedResources(), function(x){ return x.language == hash.language });
        if(localizedResource) localizedResource.id(hash.id);
      }, this);
  }

  Resource.prototype.removeWithConfirm = function() {
    if (confirm("Are you sure you want to remove " + (this.name() || "this resource") + "?")) {
      this.remove()
    }
  }

  Resource.prototype.remove= function() {
    if(this.id()) {
      var data = { id: this.id() };
      data._method = 'delete'
      $.post("/projects/" + project_id + "/resources/" + this.id(), data)
    }
    project.removeResource(this)
  }
})
