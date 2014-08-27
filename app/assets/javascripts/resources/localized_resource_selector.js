#= require resources/text_localized_resource
#= require resources/url_localized_resource
#= require resources/record_localized_resource
#= require resources/upload_localized_resource

onResourcesWorkflow(function(){
  window['LocalizedResourceSelector']= function LocalizedResourceSelector(options, resource){
    var self = this;

    this.options = ko.observableArray(options);
    this.current = ko.observable(options[0]);

    this.parent = resource;
    this.title = ko.observable('');

    this.is_text = ko.computed((function(_this) {
      return function() {
        var _ref;
        return ((_ref = _this.current()) != null ? _ref.type() : void 0) === "TextLocalizedResource";
      };
    })(this));


    this.isValid = ko.computed(function(){
      return this.current() && this.current().isValid()
    }, this);

    this.with_title = function(new_title) {
      this.title(new_title);
      return this;
    };

    this.with_language = function(language) {
      this.language(language);
      return this;
    };

    this.with_parent = function(parent) {
      var option, _i, _len, _ref;
      _ref = this.options();
      for (_i = 0, _len = _ref.length; _i < _len; _i++) {
        option = _ref[_i];
        option.set_parent(parent);
      }
      return this;
    };
  }

  LocalizedResourceSelector.prototype.editing = function(value){
    if(value){
      this.parent.editing(value);
    }
    return this.parent.editing();
  }

  LocalizedResourceSelector.prototype.language = function(){
    return this.current().language();
  }

  LocalizedResourceSelector.prototype.uploadStatus = function(){
    return this.current().uploadStatus();
  }

  LocalizedResourceSelector.prototype.toHash = function(){
    return this.current().toHash();
  }

  LocalizedResourceSelector.prototype.beforeSave = function(){
    this.current().beforeSave();
  }

  LocalizedResourceSelector.prototype.afterSave = function(){
    this.current().afterSave();
  }

  LocalizedResourceSelector.prototype.afterSaveFailed = function() {
    this.current().afterSaveFailed();
  }

  LocalizedResourceSelector.fromHash = function(hash, resource){
    var self = this;
    options = _.map(this.prototype.optionsArray(), function(type){ return new window[type + "LocalizedResource"](hash, resource) });
    selector = new LocalizedResourceSelector(options, resource);

    selector.current(_.detect(options, function(option){ return option.type() == hash.type }) || _.detect(options, function(option){ return option.type() == self.prototype.defaultOptionType() }));
    return selector
  }

  LocalizedResourceSelector.prototype.template = function() {
    return 'localized_resource_selector_template'
  }

  LocalizedResourceSelector.prototype.preserveCurrentValues = function() {
    this.original_current = this.current();
    _.each(this.options(), function(localized) {localized.preserveCurrentValues()})
  }

  LocalizedResourceSelector.prototype.revertToPreservedValues = function() {
    if(this.original_current) {
      this.current(this.original_current);
    }
    _.each(this.options(), function(localized) {localized.revertToPreservedValues()})
  }
})

onResources(function(){
  LocalizedResourceSelector.prototype.optionsArray = function(){
    return ['Undefined', 'Text', 'Url', 'Record', 'Upload'];
  }

  LocalizedResourceSelector.prototype.defaultOptionType = function(){
    return "UndefinedLocalizedResource"
  }
})

onWorkflow(function(){
  LocalizedResourceSelector.prototype.optionsArray = function(){
    return ['Text', 'Url', 'Record', 'Upload'];
  }

  LocalizedResourceSelector.prototype.defaultOptionType = function(){
    return "TextLocalizedResource"
  }
})
