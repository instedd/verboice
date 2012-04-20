#= require workflow/steps/step

onWorkflow ->
  class window.New extends Step
    constructor: (attrs={}) ->
      super(attrs)
      @parent = null

    button_class: () =>
      'ltext'

    default_name: () =>
      "Add new step"

    set_as_current: () =>
      workflow.sidebar_content(workflow.command_selector())
      workflow.current_step(@)

    can_add_next: () =>
      false

    to_hash: () =>
      null

    is_serializable: () =>
      false

    clear: () =>
      if @parent? && @parent.next_id == @id
        workflow.current_step().next_id = null
        workflow.steps.remove(@)
        @parent.next_id = null

    current_step: (step) =>
      if step and step.can_add_next()
        step.next_id = @id
        @parent = step
        workflow.steps.push(@)


