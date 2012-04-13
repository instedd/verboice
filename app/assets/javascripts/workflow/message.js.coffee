onWorkflow ->
  class window.Message
    constructor: (hash={}) ->
      @name = ko.observable hash.name
      @title = ko.observable ""
      @type = ''
      @parent = null

    to_hash: () =>
      name: @name()
      type: @type

    @from_hash: (hash) ->
      #HACK: Handle null case with a base class message
      return new RecordedMessage if not hash? || not hash.type?
      switch hash.type.toLowerCase()
        when 'record', 'recording'
          new RecordedMessage(hash)
        else
          throw "Message type not recognised #{hash['type']}"

    with_title: (new_title) =>
      @title(new_title)
      return @

    with_parent: (new_parent) =>
      @parent = new_parent
      return @

    back: () =>
      @parent.current_editing_message(null)
