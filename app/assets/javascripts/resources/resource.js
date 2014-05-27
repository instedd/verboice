#= require resources/localized_resource_selector

console.log("pasamos por resource.js");
onResourcesWorkflow(function(){
  window['Resource']= function Resource(hash, project){

    var self = this;
    this.id = ko.observable(null);
    this.guid = ko.observable(null);
    this.name = ko.observable(hash['name'] || null);
    this.editing = ko.observable(false);
    this.saving = ko.observable(false);
    this.uploadError = ko.observable(false);

    var existing_localized_resources = hash['localized_resources'] || [];
    if(project){
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
    } else {
      var unpack_localized_resources = (function(_this) {
        return function(localized_resources) {
          localized_resources = localized_resources || [];
          return _.map(project_languages, function(l) {
            var localized_resource;
            localized_resource = _.find(localized_resources, function(lr) {
              return lr.language === l.key;
            });
            localized_resource || (localized_resource = {
              language: l.key
            });
            return LocalizedResourceSelector.fromHash(localized_resource, self).with_title(l.value).with_language(l.key).with_parent(_this);
          });
        };
      })(this);

      this.localizedResources = ko.observableArray(unpack_localized_resources(hash.localized_resources));
    }

    this.current_editing_localized_resource = ko.observable(this.localizedResources()[0]);


    this.is_valid = ko.computed(function() {
      return (this.name() && _.all(self.localizedResources(), function(x) {
          return x.isValid();
        }));
    }, this);

    this.is_text = ko.computed((function(_this) {
      return function() {
        return _.all(self.localizedResources(), function(x) {
          return x.is_text();
        });
      };
    })(this));

    this.edit = function(resource){
      debugger
      // if (this.editing()) {
      //   return true;
      // } else {
        self.editing(true);
        self.preserveCurrentValues();
        self.current_editing_localized_resource(resource)
      // }
    }
  }

  Resource.find = function(guid, callback) {
    return $.getJSON("/projects/" + project_id + "/resources/find.json?guid=" + guid, function(data) {
      return typeof callback === "function" ? callback(new Resource(data)) : void 0;
    });
  };

  Resource.search = function(q, callback) {
    return $.getJSON("/projects/" + project_id + "/resources.json?q=" + q, function(data) {
      var i;
      return typeof callback === "function" ? callback((function() {
        var _i, _len, _results;
        _results = [];
        for (_i = 0, _len = data.length; _i < _len; _i++) {
          i = data[_i];
          _results.push(new Resource(i));
        }
        return _results;
      })()) : void 0;
    });
  };

  Resource.fromHash = function(hash, project){
    var resource = new Resource(hash, project);

    resource.id(hash['id']);
    resource.guid(hash['guid']);
    resource.name(hash['name']);

    return resource;
  }

  Resource.prototype.save = function(callback){
    if(! this.is_valid()) {
      return false;
    };
    var self = this;
    var data = this.toHash();
    self.beforeSave();
    self.saving(true);
    self.uploadError(false);
    debugger
    if(this.id()) {
      $.ajax({
        type: 'PUT',
        url: "/projects/" + project_id + "/resources/" + this.id() + ".json",
        contentType: 'application/json',
        data: JSON.stringify(data),
        success: function(response){
          console.log("en el success 2");
          self.updateLocalizedResources(response.localized_resources);
          self.afterSave();
          self.editing(false);
        },
        error: function(error) {
          console.log("en el error 2");
          self.uploadError(true);
        },
        complete: function() {
          console.log("en el complete 2");
          self.saving(false);
        }
      });
    } else {
      $.ajax({
        type: 'POST',
        url: "/projects/" + project_id + "/resources.json",
        contentType: 'application/json',
        data: JSON.stringify(data),
        success: function(response){
          debugger
          console.log("en el success 1");
          self.id(response.id);
          self.guid(response.guid);
          self.updateLocalizedResources(response.localized_resources);
          self.afterSave();
          self.editing(false);
          return typeof callback === "function" ? callback(self) : void 0;
        },
        error: function(error) {
          console.log("en el error 1");
          self.uploadError(true);
        },
        complete: function() {
          console.log("en el complete 1");
          self.saving(false);
        }
      });
    };
  }

  Resource.prototype.cancel = function(){
    this.editing(false);
    this.uploadError(false);
    this.revertToPreservedValues();
    if (! this.id() ) { this.remove() };
  }

  Resource.prototype.beforeSave = function(){
    _.each(this.localizedResources(), function(localized) {localized.beforeSave()});
  }

  Resource.prototype.afterSave = function(){
    _.each(this.localizedResources(), function(localized) {localized.afterSave()});
  }

  Resource.prototype.preserveCurrentValues= function() {
    this.original_name = this.name();
    _.each(this.localizedResources(), function(localized) {localized.preserveCurrentValues()});
  }

  Resource.prototype.revertToPreservedValues= function() {
    this.name(this.original_name);
    _.each(this.localizedResources(), function(localized) {localized.revertToPreservedValues()});
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
        localizedResource = _.detect(this.localizedResources(), function(x){
          // debugger
          return x.language == hash.language });
        if(localizedResource) {
          localizedResource.current().id(hash.id);
        }
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
