#= require workflow/resources/localized_resource

onWorkflow ->
  class window.UrlLocalizedResource extends LocalizedResource

    constructor: (hash = {}) ->
      super(hash)

      @url = ko.observable hash.url

    to_hash: =>
      $.extend(super,
        url: @url()
      )