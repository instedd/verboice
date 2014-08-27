#= require resources/resource
onWorkflow ->
  class window.ResourceEditor

    constructor: (parent, hash = {}) ->
      @parent = parent
      @resource = ko.observable null
      @type = ko.observable 'new'
      @name = ko.observable null
      @query = ko.observable null
      @existing_resource_guid = ko.observable null
      @matching_resources = ko.observableArray()

      @display_name = ko.computed =>
        if @resource()? then @resource().name() else null

      @is_next_enabled = ko.computed =>
        if @type() == 'new'
          @name()? and @name() != ''
        else
          @existing_resource_guid()?

      if hash.guid?
        Resource.find hash.guid, (result) =>
          @resource(result)
      @is_valid = ko.computed =>
        if @resource()? then @resource().is_valid() else false
      @is_text = ko.computed =>
        if @resource()? then @resource().is_text() else false


    get_resources: (query, source) =>
      Resource.search query, (results) =>
        source(results)

    cancel: =>
      if @resource()
        _.each(@resource().localizedResources(), (localized) => localized.current().uploadStatus('standBy'))
      @parent.current_editing_resource(null)

    next: =>
      switch @type()
        when 'new'
          @resource(new Resource(name: @name()))
        when 'existing'
          Resource.find @existing_resource_guid(), (result) =>
            @resource(result)

    save: =>
      @resource().save()
      subscription = @resource().uploadOk.subscribe (upload_ok) =>
        # if the resource finished saving and there without any errors, the edit window needs to be closed
        if(upload_ok)
          @cancel()

          # We no longer want this event to be triggered again
          subscription.dispose()

    replace: =>
      @resource(null)

    to_hash: =>
      if @resource()?.guid()?
        { guid: @resource().guid() }
      else
        {}

    show_language: (language) =>
      @resource().show_language(language)
