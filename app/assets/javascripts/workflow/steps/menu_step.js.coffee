#= require workflow/steps/step_with_children
#= require workflow/steps/menu_option

onWorkflow ->
  class window.Menu extends StepWithChildren
    @type = 'menu'

    constructor: (attrs) ->
      super(attrs)

      @options = ko.observableArray([])
      @new_option_command = ko.observable null
      @default_command_selected = ko.observable 'skip'

      @current_editing_message = ko.observable null
      @timeout = ko.observable attrs.timeout ? menu_default_time_out_in_seconds
      @number_of_attempts = ko.observable attrs.number_of_attempts ? menu_default_number_of_attempts

      @store = ko.observable attrs.store
      @defines_store = ko.observable !!attrs.store

      @message_selectors =
        invalid:     MessageSelector.from_hash(attrs.invalid_message).with_title('Invalid').with_parent(@)
        explanation: MessageSelector.from_hash(attrs.explanation_message).with_title('Explanation').with_parent(@)
        options:     MessageSelector.from_hash(attrs.options_message).with_title('Options').with_parent(@)

      @is_editing_message = ko.computed () =>
        @current_editing_message() != null

      @available_numbers = ko.computed () =>
        used_numbers = (opt.number() for opt in @options())
        (number for number in ['1','2','3','4','5','6','7','8','9','0','#','*'] when number not in used_numbers)

      @default_id = attrs.default

    default: () =>
      workflow.get_step(@default_id)

    button_class: () =>
      'ldial'

    @add_to_steps: () ->
      workflow.add_step(new Menu)

    @initialize: (hash) ->
      menu = new Menu(hash)
      menu.options(new MenuOption(opt.number, opt.next, menu) for opt in (hash.options || []))
      return menu

    to_hash: () =>
      $.extend(super,
        store: (if @defines_store() then @store() else null)
        options: (option.to_hash() for option in @options())
        invalid_message: @message_selectors['invalid'].to_hash()
        explanation_message: @message_selectors['explanation'].to_hash()
        options_message: @message_selectors['options'].to_hash()
        timeout: @timeout()
        number_of_attempts: @number_of_attempts()
        default: @default_id
      )

    add_option: () =>
      new_step_id = @new_child_step_for @new_option_command()
      @options.push(new MenuOption(@available_numbers()[0], new_step_id, @))

    remove_option_with_confirm: (option) =>
      if confirm("Are you sure you want to remove option #{option.number()} and all its steps?")
        @remove_child_step(option)

    remove_child_step: (child_step) =>
      super(child_step)
      @options.remove child_step

    child_steps: () =>
      child = @options().sort((opt1, opt2) => opt1.number().charCodeAt(0) - opt2.number().charCodeAt(0))

    children: () =>
      if @default_id
        super.concat(@default())
      else
        super

    message: (msg) =>
      @message_selectors[msg]

    show_message: (msg) =>
      msg = @message_selectors[msg]
      @current_editing_message(msg)


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

    show_invalid_message: () =>
      @show_message('invalid')

    show_options_message: () =>
      @show_message('options')

    show_explanation_message: () =>
      @show_message('explanation')

    after_initialize: () =>
      if @default_id
        @default_command_selected(@default().type())
