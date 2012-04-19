onWorkflow ->
  class window.Workflow
    constructor: (command_selector) ->
      @steps = ko.observableArray(Step.from_hash(hash) for hash in application_flow)
      @command_selector = ko.observable(command_selector)
      @add_new_step = new window.New({id: -1})

      @current_step = ko.observable(null)
      @sidebar_content = ko.observable(command_selector)

    get_step: (id) =>
      return null if not id?
      (step for step in @steps() when step.id == id)[0]

    get_parent: (step) =>
      (parent for parent in @steps() when step.id in parent.next_ids())[0]

    add_step: (command) =>
      @steps.push command

    create_step: (command_type, parent) =>
      new_step = Step.from_hash(type: command_type, id: @generate_id(), root: not parent?)
      parent.next_id = new_step.id if parent?
      @steps.push new_step
      new_step

    remove_step: (step) =>
      @steps.remove(step)
      @show_new_step_selector() if @current_step() == step

    set_as_current: (step) =>
      if @current_step()?.next_id == @add_new_step.id
        @current_step().next_id = null
        @steps.remove(@add_new_step)

      @sidebar_content(step || @command_selector())
      @current_step(step)

      if step and step.can_add_next()
        step.next_id = @add_new_step.id
        @add_new_step.parent = step
        @steps.push(@add_new_step)

    show_new_step_selector: () =>
      @set_as_current(null)

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

    generate_id: () =>
      id = new Date().getTime()
      while id in (step.id for step in @steps())
        id += 1
      return id