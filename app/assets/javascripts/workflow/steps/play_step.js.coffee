#= require workflow/steps/step

onWorkflow ->
  class window.Play extends Step
    @type = 'play'

    constructor: (attrs) ->
      super(attrs)

      @next_id = attrs.next
      @message = MessageSelector.from_hash(attrs.message).with_title('Message').with_parent(@)
      @current_editing_message = ko.observable(null)

      @is_editing_message = ko.computed () =>
        @current_editing_message() != null

    button_class: () =>
      'lsound'

    @add_to_steps: () ->
      workflow.add_step(new Play)

    @initialize: (hash) ->
      step = new Play(hash)
      return step

    to_hash: () =>
      $.extend(super,
        message: @message.to_hash()
      )

    show_message: () =>
      @current_editing_message(@message)
