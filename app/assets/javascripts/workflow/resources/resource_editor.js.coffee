#= require workflow/resources/resource

onWorkflow ->
  class window.ResourceEditor

    constructor: (parent) ->
      @parent = parent

      @resource = ko.observable null
      @type = ko.observable 'new'
      @name = ko.observable null
      @query = ko.observable null
      @existing_resource_id = ko.observable null
      @matching_resources = ko.observableArray()

      @is_next_enabled = ko.computed =>
        if @type() == 'new'
          @name()? and @name() != ''
        else
          @existing_resource_id()?

    get_resources: (query, source) =>
      Resource.search query, (results) =>
        source(results)

    cancel: =>
      @parent.current_editing_resource(null)

    next: =>
      switch @type()
        when 'new'
          @resource(new Resource(name: @name()))
        when 'existing'
          Resource.find @existing_resource_id(), (result) =>
            @resource(result)

    save: =>
      @resource().save =>
        @cancel()

    replace: =>
      @resource(null)

    to_hash: =>
      if @resource()? && @resource().id()?
        { id: @resource().id() }
      else
        {}

    is_valid: =>
      true
