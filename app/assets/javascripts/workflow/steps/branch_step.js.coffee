#= require workflow/steps/step_with_children
#= require workflow/steps/branch_option

onWorkflow ->
  class window.Branch extends StepWithChildren
    @type = 'branch'

    constructor: (attrs) ->
      super(attrs)

      @options = ko.observableArray([])
      @new_option_command = ko.observable null

      @default = ko.observable( new DefaultOption(null, @))

      @current_editing_option = ko.observable null
      @is_editing_option = ko.computed () =>
        @current_editing_option() != null

      @are_options_invalid = ko.computed () =>
        (@options().length < 1)

      @is_invalid = ko.computed () =>
        @is_name_invalid() or @are_options_invalid()

    button_class: () =>
      'ldirections'

    can_insert_after: () =>
      false

    @add_to_steps: () ->
      workflow.add_step(new Branch)

    @initialize: (hash) ->
      branch = new Branch(hash)

      for opt in (hash.options || [])
        if opt.is_default
          branch.default(new DefaultOption(opt.next, branch))
        else
          branch.options.push(new BranchOption(opt.conditions, opt.next, branch))

      return branch

    to_hash: () =>
      $.extend(super,
        options: (option.to_hash() for option in @child_steps())
      )

    default_name: () =>
      'Branches'

    add_option: () =>
      new_step_id = @new_child_step_for @new_option_command()
      @options.push(new BranchOption([], new_step_id, @))

    remove_option_with_confirm: (option) =>
      if confirm("Are you sure you want to remove this option and all its steps?")
        @remove_child_step(option)

    remove_child_step: (child_step) =>
      super(child_step)
      if child_step.is_default
        @default_command_selected('skip')
      else
        @options.remove child_step

    child_steps: () =>
      @options().concat(@default())

    move_option_up: (option) =>
      index = @options.indexOf option
      if index > 0
        before = @options()[index - 1]
        @options.splice(index - 1, 2, option, before)

    move_option_down: (option) =>
      index = @options.indexOf option
      last = @options().length - 1
      if index < last
        after = @options()[index + 1]
        @options.splice(index, 2, after, option)

    after_initialize: () =>
      super
      option.after_initialize() for option in @options()

    show_option: (option) =>
      option.begin_edition()
      @current_editing_option(option)

    on_step_removed: (step) =>
      option.on_step_removed(step) for option in @options()
