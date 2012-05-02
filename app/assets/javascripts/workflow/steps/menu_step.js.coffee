#= require workflow/steps/step
#= require workflow/steps/menu_option

onWorkflow ->
  class window.Menu extends Step
    @type = 'menu'

    constructor: (attrs) ->
      super(attrs)

      @options = ko.observableArray([])
      @new_option_command = ko.observable null
      @current_editing_message = ko.observable null

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

    commands: () =>
      (step_type.type for step_type in step_types).concat(['skip'])

    @add_to_steps: () ->
      workflow.add_step(new Menu)

    @initialize: (hash) ->
      menu = new Menu(hash)
      menu.options(new MenuOption(opt.number, opt.next, menu) for opt in (hash.options || []))
      return menu

    to_hash: () =>
      $.extend(super,
        options: (option.to_hash() for option in @options())
        end_call_message: @message_selectors['end_call'].to_hash()
        invalid_message: @message_selectors['invalid'].to_hash()
        explanation_message: @message_selectors['explanation'].to_hash()
        options_message: @message_selectors['options'].to_hash()
      )

    add_option: () =>
      new_step_id = if (@new_option_command() == 'skip') then null else workflow.create_step(@new_option_command(), false).id
      @options.push(new MenuOption(@available_numbers()[0], new_step_id, @))

    option_for: (step) =>
      for option in @options()
        if option.next_id == step.id
          return option

    remove_option_with_confirm: (option) =>
      if confirm("Are you sure you want to remove option #{option.number()} and all its steps?")
        @remove_option(option)

    remove_option: (option) =>
      @options.remove option
      option.remove_next()

    remove_with_confirm: () =>
      name = @name?() || "this step"
      if confirm("Are you sure you want to remove #{name}?")
        @remove()

    remove: () =>
      for option in @options()
        option.remove_next()
      super()

    children: () =>
      (option.next() or option.skip() for option in @sorted_options())

    children_ids: () =>
      (option.next_id for option in @sorted_options())

    leaves: () =>
      if @next()?
        @next().leaves()
      else if @children()? && @children().length > 0
        [].concat.apply([], (child.leaves() for child in @children()))
      else
        [@]

    sorted_options: () =>
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


  class window.Skip extends Step
    @type = 'skip'

    constructor: () ->
      super({})
      @id = null

    leaves: () =>
      [@]

    next: () =>
      null

  window['skip'] = Skip

