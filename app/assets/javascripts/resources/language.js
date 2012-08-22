onResources(function(){
  window['Language']= function Language(hash){
    this.iso = ko.observable(hash.key);
    this.label = ko.observable(hash.value);
  }

  Language.fromHash = function(hash){
    return new Language(hash);
  }

})
