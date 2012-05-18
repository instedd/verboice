#= require workflow/steps/step

onWorkflow ->
  class window.StepWithChildren extends Step
    constructor: (attrs) ->
      super(attrs)
      @default_command_selected = ko.observable 'skip'

    commands: () =>
      (step_type.type for step_type in step_types).concat(['skip'])

    new_child_step_for: (command) =>
      if (command == 'skip') then null else workflow.create_step(command, false).id

    child_step_for: (step) =>
      for child_step in @child_steps()
        if child_step.next_id == step.id
          return child_step

    remove_child_step: (child_step) =>
      child_step.remove_next()

    remove_with_confirm: () =>
      name = @name?() || "this step"
      if confirm("Are you sure you want to remove #{name}?")
        @remove()

    remove: () =>
      for child_step in @child_steps()
        child_step.remove_next()
      super()

    children: () =>
      (option.next() or option.skip() for option in @child_steps())

    children_ids: () =>
      (option.next_id for option in @child_steps())

    leaves: () =>
      if @next()?
        @next().leaves()
      else if @children()? && @children().length > 0
        [].concat.apply([], (child.leaves() for child in @children()))
      else
        [@]

    child_updated: (previous_step, new_step) =>
      @default_command_selected(new_step.type())

    change_default_option: (command) =>
      @default().remove_next()
      @default(new DefaultOption(@new_child_step_for(@default_command_selected()), @))

    after_initialize: () =>
      @default_command_selected(@default().type())