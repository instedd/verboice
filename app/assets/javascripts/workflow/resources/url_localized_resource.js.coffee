#= require workflow/resources/localized_resource

onWorkflow ->
  class window.UrlLocalizedResource extends LocalizedResource

    constructor: (hash = {}) ->
      super(hash)

      @label = 'Online resource'
      @template = 'url_localized_resource_template'
      @url = ko.observable hash.url

      @is_valid = ko.computed =>
        @url()? and @url().length > 0

    to_hash: =>
      $.extend(super,
        url: @url()
      )

    type: () =>
      'UrlLocalizedResource'
