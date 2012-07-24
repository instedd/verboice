#= require workflow/resources/localized_resource

onWorkflow ->
  class window.TextLocalizedResource extends LocalizedResource

    constructor: (hash = {}) ->
      super(hash)

      @text = ko.observable hash.text

    to_hash: =>
      $.extend(super,
        text: @text()
      )