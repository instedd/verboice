#= require workflow/steps/step
#= require workflow/steps/branch_option

onWorkflow ->
  class window.Branch extends Step
    @type = 'branch'

    constructor: (attrs) ->
      super(attrs)

      @next_id = attrs.next
      @options = ko.observableArray([])
      @else_option = ko.observable null
      @new_option_command = ko.observable null
      @new_else_option_command = ko.observable null
      @has_else = ko.computed () =>
        @else_option() != null
      @all_options = ko.computed () =>
        opts = @options.slice(0)
        opts.push(@else_option()) if @has_else()
        opts

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
        options: (option.to_hash() for option in @all_options())
      )

    default_name: () =>
      'Branches'

    commands: () =>
      (step_type.type for step_type in step_types).concat(['skip'])

    add_option: () =>
      new_step_id = @new_step_for(@new_option_command())
      @options.push(new BranchOption([], new_step_id, @))

    add_else_option: () =>
      new_step_id = @new_step_for(@new_else_option_command())
      @else_option(new BranchElseOption(new_step_id, @))

    new_step_for: (command) =>
      if (command == 'skip') then null else workflow.create_step(command, false).id

    option_for: (step) =>
      for option in @all_options()
        if option.next_id == step.id
          return option

    remove_option_with_confirm: (option) =>
      @remove_option(option) if @confirm_remove()

    remove_else_option_with_confirm: (option) =>
      @remove_else_option() if @confirm_remove()

    confirm_remove: () =>
      confirm("Are you sure you want to remove this option and all its steps?")

    remove_option: (option) =>
      @options.remove option
      option.remove_next()

    remove_else_option: () =>
      option = @else_option()
      @else_option(null)
      option.remove_next()

    remove_with_confirm: () =>
      name = @name?() || "this step"
      if confirm("Are you sure you want to remove #{name}?")
        @remove()

    remove: () =>
      for option in @all_options()
        option.remove_next()
      super()

    children: () =>
      (option.next() or option.skip() for option in @all_options())

    children_ids: () =>
      (option.next_id for option in @all_options())

    leaves: () =>
      if @next()?
        @next().leaves()
      else if @children()? && @children().length > 0
        [].concat.apply([], (child.leaves() for child in @children()))
      else
        [@]

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
