onWorkflow ->
  class window.LocalizedResource

    constructor: (hash = {}) ->
      @id = ko.observable hash.id
      @language = ko.observable hash.language
      @parent = ko.observable null

    to_hash: =>
      id: @id()
      language: @language()
      type: @type()

    type: () =>
      @.constructor.name

    set_parent: (parent) =>
      @parent(parent)

    is_saved: () =>
      @parent()?.id()? and @id()?
