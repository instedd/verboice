onWorkflow ->
  class window.CommandSelector
    constructor: (requestor) ->
      @handlers = (new ClassBindingHandler(klass, @) for klass in @enabled_command_types())
      @commands = ko.observableArray(@handlers)
      @requestor = requestor

      @standard_commands = ko.computed =>
        (command for command in @commands() when !command.cmd.is_external?())

      @external_commands = ko.computed =>
        (command for command in @commands() when command.cmd.is_external?())

    display_template_id: () ->
      'command_selector_template'

    with_requestor: (requestor) =>
      @requestor = requestor
      if requestor.excluded_types?
        @commands(handler for handler in @handlers when handler.cmd not in requestor.excluded_types)
      else
        @commands(@handlers)
      return @

    enabled_command_types: () ->
      if nuntium_configured
        step_types
      else
        klass for klass in step_types when klass isnt Nuntium

  class window.AddRootRequestor
    command_selected: (cmd_type) =>
      new_step = workflow.create_step(cmd_type, null)
      workflow.set_as_current(new_step)

  class window.AddNextRequestor
    constructor: (parent) ->
      @parent = parent

    command_selected: (cmd_type) =>
      new_step = workflow.create_step(cmd_type, @parent)
      workflow.set_as_current(new_step)

  class window.InsertBeforeRequestor
    constructor: (step) ->
      @step = step
      @excluded_types = [Goto, HangUp]

    command_selected: (cmd_type) =>
      parent = @step.parent()
      new_step = workflow.create_step cmd_type, parent, (step) =>
        step.next_id = @step.id
        step.root = @step.root
        @step.root = false
        workflow.set_as_current(step)

  class window.InsertAfterRequestor
    constructor: (step) ->
      @step = step
      @excluded_types = [Goto, HangUp]

    command_selected: (cmd_type) =>
      next_id = @step.next_id
      new_step = workflow.create_step cmd_type, @step, (new_step) =>
        new_step.next_id = next_id
        workflow.set_as_current(new_step)

