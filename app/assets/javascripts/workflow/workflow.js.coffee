onWorkflow ->
  class window.Workflow
    constructor: (command_selector) ->
      @steps = ko.observableArray(Step.from_hash(hash) for hash in application_flow)
      @command_selector = ko.observable(command_selector)
      @current_step = ko.observable @command_selector()

    get_step: (id) =>
      (step for step in @steps() when step.id == id)[0]

    get_parent: (step) =>
      (parent for parent in @steps() when step.id in parent.next_ids())[0]

    add_step: (command) =>
      @steps.push command

    create_step: (command_type, parent) =>
      new_step = Step.from_hash(type: command_type, id: @generate_id(), root: not parent?)
      @steps.push new_step
      new_step

    remove_step: (step) =>
      @steps.remove(step)
      @initialize_current_step() if @current_step() == step

    set_as_current: (step) =>
      @current_step(step)

    initialize_current_step: () =>
      @set_as_current(@command_selector())

    display_template_for: (current_flow_step) =>
      @current_step().display_template_id()

    serialize_workflow: () =>
      serialized = JSON.stringify(step.to_hash() for step in @steps())
      $('#flow').val(serialized)
      return true # let the submit handler do its work

    commands: () =>
      @command_selector().commands()

    # private

    generate_id: () =>
      id = new Date().getTime()
      while id in (step.id for step in @steps())
        id += 1
      return id