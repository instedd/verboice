onWorkflow ->
  class window.MessageSelector
    constructor: (msgs=[]) ->
      @messages = ko.observableArray(msgs)
      @current_message = ko.observable(msgs[0])
      @title = ko.observable('')
      @parent = null
      @name = ko.computed () =>
        @current_message()?.name()

    to_hash: () =>
      hash = @current_message().to_hash()
      if hash?
        $.extend hash,
          title: @title()
      else
        {}

    @from_hash: (hash) ->
      msgs = [text, recording] = [new TextMessage(hash), new RecordedMessage(hash)]
      selector = new MessageSelector(msgs)

      if hash? and hash.type?
        current = switch hash.type.toLowerCase()
            when 'record', 'recording'
              recording
            when 'text'
              text
        selector.current_message(current)

      return selector

    with_title: (new_title) =>
      @title(new_title)
      for msg in @messages()
        msg.title = new_title
      return @

    with_parent: (new_parent) =>
      @parent = new_parent
      for msg in @messages()
        msg.parent = new_parent
      return @

    back: () =>
      @current_message().exit()
      @parent.current_editing_message(null)

    message: () =>
      @current_message()