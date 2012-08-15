#= require workflow/resources/localized_resource

onWorkflow ->
  class window.TextLocalizedResource extends LocalizedResource

    constructor: (hash = {}) ->
      super(hash)

      @label = 'Text to speech'
      @template = 'text_localized_resource_template'
      @text = ko.observable hash.text

      @is_valid = ko.computed =>
        @text()? and @text().length > 0

    to_hash: =>
      $.extend(super,
        text: @text()
      )

    type: () =>
      'TextLocalizedResource'
