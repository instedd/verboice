#= require workflow/resources/text_localized_resource
#= require workflow/resources/url_localized_resource
#= require workflow/resources/record_localized_resource
#= require workflow/resources/upload_localized_resource

onWorkflow ->
  class window.LocalizedResourceSelector

    constructor: (options = []) ->
      @options = ko.observableArray options
      @current = ko.observable options[0]
      @title = ko.observable ''
      @language = null

      @is_valid = ko.computed =>
        @current()?.is_valid()

    to_hash: () =>
      @current().to_hash()

    @from_hash: (hash) ->
      options = (new window["#{type}LocalizedResource"](hash) for type in ['Text', 'Url', 'Record', 'Upload'])
      selector = new LocalizedResourceSelector(options)

      for option in options
        selector.current(option) if option.type() == hash.type

      return selector

    with_title: (new_title) =>
      @title(new_title)
      return @

    with_language: (language) =>
      @language = language
      return @

    with_parent: (parent) =>
      for option in @options()
        option.set_parent(parent)
      return @

    guid: (guid) =>
      for option in @options()
        option.guid(guid) unless option.guid()?
