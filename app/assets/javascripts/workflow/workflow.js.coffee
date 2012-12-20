onWorkflow ->
  class window.Workflow
    constructor: () ->
      @steps = ko.observableArray(Step.from_hash(hash) for hash in call_flow)
      @command_selector = new CommandSelector(new AddRootRequestor)
      @add_new_step = new window.New({id: -1})

      @current_step = ko.observable(null)
      @sidebar_content = ko.observable(@command_selector)
      @call_simulator = new CallSimulator(@)
      @is_valid = ko.computed () =>
        (return false for step in @steps() when step.is_invalid())
        true

    get_step: (id) =>
      return null if not id?
      (step for step in @steps() when step.id == id)[0]

    get_parent: (step) =>
      (parent for parent in @steps() when (step.id == parent.next_id))[0] or (parent.child_step_for(step) for parent in @steps() when parent.children_ids? and step.id in parent.children_ids())[0]

    add_step: (command) =>
      @steps.push command

    create_step: (command_type, parent, callback) =>
      new_step = Step.from_hash(type: command_type, root: not parent?)
      parent.next_id = new_step.id if parent?
      callback(new_step) if callback?
      @steps.push new_step
      new_step.after_initialize()
      new_step

    remove_step: (step_to_remove) =>
      for step in @steps()
        step.on_step_removed(step_to_remove) if step.on_step_removed
      @steps.remove(step_to_remove)
      @set_as_current(null) if @current_step() == step_to_remove

    set_as_current: (step) =>
      @sidebar_content(step || @command_selector.with_requestor(new AddRootRequestor))
      @current_step(step)
      @add_new_step.current_step(step)

    show_new_step_selector: () =>
      @set_as_current(null)

    call_from_browser: =>
      return if @sidebar_content() == @call_simulator

      @old_sidebar_content = @sidebar_content()
      @old_current_step = @current_step()
      @sidebar_content(@call_simulator)
      @call_simulator.start()

    call_simulator_ended: =>
      @sidebar_content(@old_sidebar_content)
      @current_step(@old_current_step)

    show_command_selector: (requestor) =>
      @sidebar_content(@command_selector.with_requestor(requestor or new AddRootRequestor))

    display_template_for: () =>
      @sidebar_content().display_template_id()

    serialize: () =>
      JSON.stringify(step.to_hash() for step in @steps() when step.is_serializable())

    serialize_workflow: () =>
      serialized = @serialize()
      $('#flow').val(serialized)
      return true # let the submit handler do its work

    has_changed: () =>
      $('#flow').val() != @serialize()

    commands: () =>
      @command_selector.commands()

    variables: () =>
      vars = []
      for step in @steps()
        if step.defines_store?()
          store = step.store()
          if store instanceof Array
            vars = vars.concat(store)
          else
            vars.push(store)
      return vars

    all_variables: () =>
      @variables().sort().concat(
        variable for variable in distinct_variables.sort() when variable not in workflow.variables()
      )

    generate_id: () =>
      id = new Date().getTime()
      while id in (step.id for step in @steps())
        id += 1
      return id

    after_initialize: () =>
      for step in @steps()
        step.after_initialize()
