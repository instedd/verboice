#= require workflow/steps/step_with_children
#= require workflow/steps/menu_option

onWorkflow ->
  class window.Menu extends StepWithChildren
    @type = 'menu'

    constructor: (attrs) ->
      super(attrs)

      @options = ko.observableArray([])
      @new_option_command = ko.observable null
      @current_editing_message = ko.observable null

      @store = ko.observable attrs.store
      @defines_store = ko.observable !!attrs.store

      @message_selectors =
        end_call:    MessageSelector.from_hash(attrs.end_call_message).with_title('End call').with_parent(@)
        invalid:     MessageSelector.from_hash(attrs.invalid_message).with_title('Invalid').with_parent(@)
        explanation: MessageSelector.from_hash(attrs.explanation_message).with_title('Explanation').with_parent(@)
        options:     MessageSelector.from_hash(attrs.options_message).with_title('Options').with_parent(@)

      @is_editing_message = ko.computed () =>
        @current_editing_message() != null

      @available_numbers = ko.computed () =>
        used_numbers = (opt.number() for opt in @options())
        (number for number in ['1','2','3','4','5','6','7','8','9','0','#','*'] when number not in used_numbers)

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
        end_call_message: @message_selectors['end_call'].to_hash()
        invalid_message: @message_selectors['invalid'].to_hash()
        explanation_message: @message_selectors['explanation'].to_hash()
        options_message: @message_selectors['options'].to_hash()
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
      @options().sort((opt1, opt2) => opt1.number().charCodeAt(0) - opt2.number().charCodeAt(0))

    message: (msg) =>
      @message_selectors[msg]

    show_message: (msg) =>
      msg = @message_selectors[msg]
      @current_editing_message(msg)

    show_end_call_message: () =>
      @show_message('end_call')

    show_invalid_message: () =>
      @show_message('invalid')

    show_options_message: () =>
      @show_message('options')

    show_explanation_message: () =>
      @show_message('explanation')
