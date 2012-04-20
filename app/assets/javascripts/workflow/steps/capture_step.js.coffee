#= require workflow/steps/step

onWorkflow ->
  class window.Capture extends Step
    @type = 'capture'

    constructor: (attrs) ->
      super(attrs)

      @valid_values = ko.observable attrs.valid_values
      @finish_on_key = ko.observable attrs.finish_on_key
      @min_input_length = ko.observable attrs.min_input_length
      @max_input_length = ko.observable attrs.max_input_length

      @current_editing_message = ko.observable null

      @message_selectors =
        end_call:    MessageSelector.from_hash(attrs.end_call_message).with_title('End call').with_parent(@)
        invalid:     MessageSelector.from_hash(attrs.invalid_message).with_title('Invalid').with_parent(@)
        instructions: MessageSelector.from_hash(attrs.instructions_message).with_title('Instructions').with_parent(@)

      @is_editing_message = ko.computed () =>
        @current_editing_message() != null

    button_class: () =>
      'lnumeral'

    @add_to_steps: () ->
      workflow.add_step(new Capture)

    @initialize: (hash) ->
      capture = new Capture(hash)
      return capture

    to_hash: () =>
      $.extend(super,
        end_call_message: @message_selectors['end_call'].to_hash()
        invalid_message: @message_selectors['invalid'].to_hash()
        instructions_message: @message_selectors['instructions'].to_hash()
        min_input_length: @min_input_length()
        max_input_length: @max_input_length()
        valid_values: @valid_values()
        finish_on_key: @finish_on_key()
      )

    message: (msg) =>
      @message_selectors[msg]

    show_message: (msg) =>
      msg = @message_selectors[msg]
      @current_editing_message(msg)

    show_end_call_message: () =>
      @show_message('end_call')

    show_invalid_message: () =>
      @show_message('invalid')

    show_instructions_message: () =>
      @show_message('instructions')
