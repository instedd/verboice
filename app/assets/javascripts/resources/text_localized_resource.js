#= require resources/localized_resource

onResourcesWorkflow(function(){
  window['TextLocalizedResource']= function TextLocalizedResource(hash, resource){
    LocalizedResource.call( this, hash, resource );
    this.label = 'Text to speech';
    this.template = 'text_localized_resource_template';
    this.text = ko.observable(hash.text);
    this.isValid = ko.computed(function(){
      return this.text() != undefined && this.text().length > 0
    }, this)
  }
  TextLocalizedResource.prototype = new LocalizedResource();
  TextLocalizedResource.prototype.constructor = TextLocalizedResource;

  TextLocalizedResource.prototype.toHash= function(){
    return $.extend(LocalizedResource.prototype.toHash.call( this ), { text: this.text() })
  }
  TextLocalizedResource.prototype.type= function(){
    return 'TextLocalizedResource'
  }

  TextLocalizedResource.prototype.preserveCurrentValues= function() {
    this.original_text = this.text();
  }

  TextLocalizedResource.prototype.revertToPreservedValues= function() {
    this.text(this.original_text);
  }
})
