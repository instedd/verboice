onWorkflow ->
  class window.LocalizedResource

    constructor: (hash = {}) ->
      @guid = ko.observable hash.guid
      @language = ko.observable hash.language
      @parent = ko.observable null

    to_hash: =>
      guid: @guid()
      language: @language()
      type: @type()

    type: () =>
      @.constructor.name

    set_parent: (parent) =>
      @parent(parent)

    is_saved: () =>
      @parent()?.guid()? and @guid()?
