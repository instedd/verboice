#= require workflow/steps/step

onWorkflow ->
  class window.New extends Step
    constructor: (attrs={}) ->
      super(attrs)
      @parent = null

    button_class: () =>
      'addnew'

    default_name: () =>
      "Add new step"

    set_as_current: () =>
      workflow.show_command_selector(new AddNextRequestor(@parent)) #sidebar_content(workflow.command_selector)
      workflow.current_step(@)

    can_add_next: () =>
      false

    can_insert_before: () =>
      false

    can_insert_after: () =>
      false

    to_hash: () =>
      null

    is_serializable: () =>
      false

    clear: () =>
      if @parent? && @parent.next_id == @id
        workflow.steps.remove(@)
        @parent.next_id = null

    current_step: (step) =>
      if @parent? && @parent.next_id == @id
        @parent.next_id = null

      if step and step.can_add_next()
        step.next_id = @id
        @parent = step
        if @ in workflow.steps()
          workflow.steps.valueHasMutated()
        else
          workflow.steps.push(@)
      else
        workflow.steps.remove(@)