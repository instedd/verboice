#= require resources/localized_resource

onResources(function(){
  window['UrlLocalizedResource']= function UrlLocalizedResource(hash, resource){
    LocalizedResource.call( this, hash, resource );
    this.label = 'Online resource';
    this.template = 'url_localized_resource_template';
    this.url = ko.observable(hash.url);
    this.isValid = ko.computed(function(){
      return this.url() && this.url().length > 0
    }, this);
  }
  UrlLocalizedResource.prototype = new LocalizedResource();
  UrlLocalizedResource.prototype.constructor = UrlLocalizedResource;

  UrlLocalizedResource.prototype.toHash= function(){
    return $.extend(LocalizedResource.prototype.toHash.call( this ), { url: this.url() })
  }
  UrlLocalizedResource.prototype.type= function(){
    return 'UrlLocalizedResource'
  }
})
