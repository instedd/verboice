#= require resources/localized_resource

onResourcesWorkflow(function(){
  window['UndefinedLocalizedResource']= function UndefinedLocalizedResource(hash, resource){
    LocalizedResource.call( this, hash, resource );
    this.label = 'Select an option';
    this.template = 'undefined_localized_resource_template';
    this.isValid = ko.observable(true);
  }

  UndefinedLocalizedResource.prototype = new LocalizedResource();
  UndefinedLocalizedResource.prototype.constructor = UndefinedLocalizedResource;

  UndefinedLocalizedResource.prototype.toHash= function(){
    return undefined;
  }
  UndefinedLocalizedResource.prototype.type= function(){
    return 'UndefinedLocalizedResource'
  }

  UndefinedLocalizedResource.prototype.isValid= function(){
    return true
  }

  UndefinedLocalizedResource.prototype.preserveCurrentValues= function() {
  }

  UndefinedLocalizedResource.prototype.revertToPreservedValues= function() {
  }
})
