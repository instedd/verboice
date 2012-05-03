#= require workflow/steps/step_with_children
#= require workflow/steps/branch_option

onWorkflow ->
  class window.Branch extends StepWithChildren
    @type = 'branch'

    constructor: (attrs) ->
      super(attrs)

      @options = ko.observableArray([])
      @new_option_command = ko.observable null

      @else_option = ko.observable null
      @new_else_option_command = ko.observable null
      @has_else = ko.computed () =>
        @else_option() != null

    button_class: () =>
      'ldirections'

    @add_to_steps: () ->
      workflow.add_step(new Branch)

    @initialize: (hash) ->
      branch = new Branch(hash)
      for opt in (hash.options || [])
        if opt.is_else
          branch.else_option(new BranchElseOption(opt.next, branch))
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

    add_else_option: () =>
      new_step_id = @new_child_step_for @new_else_option_command()
      @else_option(new BranchElseOption(new_step_id, @))

    remove_option_with_confirm: (option) =>
      if confirm("Are you sure you want to remove this option and all its steps?")
        @remove_child_step(option)

    remove_child_step: (child_step) =>
      super(child_step)
      if child_step.is_else
        @else_option(null)
      else
        @options.remove child_step

    child_steps: () =>
      opts_copy = @options.slice(0)
      opts_copy.push(@else_option()) if @has_else()
      opts_copy

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
