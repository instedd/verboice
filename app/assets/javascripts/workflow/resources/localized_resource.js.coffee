onWorkflow ->
  class window.LocalizedResource

    constructor: (hash = {}) ->
      @id = ko.observable hash.id
      @language = ko.observable hash.language
      @display_name = ko.observable hash.display_name

    to_hash: =>
      id: @id()
      language: @language()
      type: @.constructor.type
