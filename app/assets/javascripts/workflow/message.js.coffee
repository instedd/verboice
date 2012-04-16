onWorkflow ->
  class window.Message
    constructor: (hash={}, parent=null) ->
      @name = ko.observable hash.name
      @parent = parent
      @title = ""
      @type = ""


    to_hash: () =>
      name: @name()
      type: @type

    exit: () =>
      null

    template: () =>
      "#{@type}_message_template"
