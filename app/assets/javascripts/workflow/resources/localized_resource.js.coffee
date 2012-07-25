onWorkflow ->
  class window.LocalizedResource

    constructor: (hash = {}) ->
      @id = ko.observable hash.id
      @language = ko.observable hash.language

    to_hash: =>
      id: @id()
      language: @language()
      type: @type()

    is_valid: () =>
      true

    type: () =>
      @.constructor.name
