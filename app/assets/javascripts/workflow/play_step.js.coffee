#= require workflow/step

onWorkflow ->
  class window.Play extends Step
    constructor: (attrs) ->
      super(attrs)

      @next_id = attrs.next
      @message = Message.from_hash(attrs.end_call_message).with_title('Message').with_parent(@)
      @current_editing_message = ko.observable(null)

      @is_editing_message = ko.computed () =>
        @current_editing_message() != null

    button_class: () =>
      'lsound'

    next_ids: () =>
      [@next_id]

    @add_to_steps: () ->
      workflow.add_step(new Play)

    @from_hash: (hash) ->
      step = new Play(hash)
      return step

    to_hash: () =>
      $.extend(super,
        message: @message.to_hash()
      )

    child_removed: (child) =>

    show_message: () =>
      @current_editing_message(@message)
