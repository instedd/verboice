#= require workflow/steps/step
#= require workflow/steps/step_with_children
#= require workflow/steps/skip_step

onWorkflow ->
  class window.Capture extends StepWithChildren
    @type = 'capture'

    constructor: (attrs) ->
      super(attrs)

      @store = ko.observable attrs.store
      @defines_store = ko.observable !!attrs.store

      @valid_values = ko.observable attrs.valid_values
      @finish_on_key = ko.observable(attrs.finish_on_key ? capture_default_finish_key)
      @min_input_length = ko.observable(attrs.min_input_length ? capture_default_minimum_input_lenght)
      @max_input_length = ko.observable(attrs.max_input_length ? capture_default_maximum_input_lenght)
      @timeout = ko.observable(attrs.timeout ? capture_default_time_out_in_seconds)
      @number_of_attempts = ko.observable(attrs.number_of_attempts ? capture_default_number_of_attempts)

      @default_command_selected = ko.observable 'skip'
      @default_id = attrs.default
      @default_skip_step = null

      @current_editing_message = ko.observable null

      @message_selectors =
        invalid:     MessageSelector.from_hash(attrs.invalid_message).with_title('Invalid').with_parent(@)
        instructions: MessageSelector.from_hash(attrs.instructions_message).with_title('Instructions').with_parent(@)

      @is_editing_message = ko.computed () =>
        @current_editing_message() != null

    get_default_skip_step: () =>
      @default_skip_step ?= new Skip()

    default: () =>
      workflow.get_step(@default_id)

    child_steps: () =>
      new Array()

    children: () =>
      if @default_id
        [@get_default_skip_step(), @default()]
      else
        new Array()

    add_default_step: () =>
      if (@default_command_selected() == 'skip')
        @default_id = null
        workflow.steps.valueHasMutated()
      else
        step = workflow.create_step(@default_command_selected(), false)
        @default_id = step.id
        step.parent = @
        if step in workflow.steps()
          workflow.steps.valueHasMutated()
        else
          workflow.steps.push(step)

    after_initialize: () =>
      if @default_id
        @default_command_selected(@default().type())

    button_class: () =>
      'lnumeral'

    @add_to_steps: () ->
      workflow.add_step(new Capture)

    @initialize: (hash) ->
      capture = new Capture(hash)
      return capture

    to_hash: () =>
      $.extend(super,
        store: (if @defines_store() then @store() else null)
        invalid_message: @message_selectors['invalid'].to_hash()
        instructions_message: @message_selectors['instructions'].to_hash()
        min_input_length: @min_input_length()
        max_input_length: @max_input_length()
        valid_values: @valid_values()
        finish_on_key: @finish_on_key()
        default: @default_id
      )

    message: (msg) =>
      @message_selectors[msg]

    show_message: (msg) =>
      msg = @message_selectors[msg]
      @current_editing_message(msg)

    show_invalid_message: () =>
      @show_message('invalid')

    show_instructions_message: () =>
      @show_message('instructions')

    default_name: () =>
      'Input'
