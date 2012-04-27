#= require workflow/steps/step

onWorkflow ->
  class window.Branch extends Step
    @type = 'branch'

    constructor: (attrs) ->
      super(attrs)

      @next_id = attrs.next
      @options = ko.observableArray([])
      @else_option = ko.observable null
      @new_option_command = ko.observable null

    button_class: () =>
      'ldirections'

    @add_to_steps: () ->
      workflow.add_step(new Branch)

    @initialize: (hash) ->
      branch = new Branch(hash)
      branch.options(new BranchOption(opt.conditions, opt.next, branch) for opt in (hash.options || []))
      return branch

    to_hash: () =>
      $.extend(super,
        options: (option.to_hash() for option in @options())
      )

    default_name: () =>
      'Branches'

    commands: () =>
      (step_type.type for step_type in step_types).concat(['skip'])

    add_option: () =>
      new_step_id = if (@new_option_command() == 'skip') then null else workflow.create_step(@new_option_command(), false).id
      @options.push(new BranchOption([], new_step_id, @))

    option_for: (step) =>
      for option in @options()
        if option.next_id == step.id
          return option

    remove_option_with_confirm: (option) =>
      if confirm("Are you sure you want to remove this option and all its steps?")
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
      (option.next() or option.skip() for option in @options())

    children_ids: () =>
      (option.next_id for option in @options())

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
