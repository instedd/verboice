onResources(function(){
  window['Project']= function Project(){

    this.resources = ko.observableArray([]);
    var self = this;
    this.languages = ko.observableArray(_.map(project_languages, function(hash){ return Language.fromHash(hash) }));

    $.getJSON("/projects/" + project_id + "/resources.json", function(project_resources){
      self.resources(_.map(project_resources, function(hash){ return Resource.fromHash(hash, self) }));
    });

    this.firstLanguage = ko.observable(null);
    this.secondLanguage = ko.observable(null);

    if(this.languages().length > 0) this.firstLanguage(this.languages()[0]);
    if(this.languages().length > 1) this.secondLanguage(this.languages()[1]);
  }

  Project.prototype.addResource = function(){
    var res = new Resource({}, this);
    res.editing(true);
    this.resources.push(res)
  }

  Project.prototype.removeResource = function(res){
    this.resources.remove(res)
  }

})
