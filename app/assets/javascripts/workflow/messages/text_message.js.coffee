#= require workflow/messages/message

onWorkflow ->
  class window.TextMessage extends Message
    constructor: (hash={}) ->
      super(hash)
      @type = 'text'
      @label = 'Text to speech'

    to_hash: () =>
      if @name()?
        super
