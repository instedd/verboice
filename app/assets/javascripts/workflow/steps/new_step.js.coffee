#= require workflow/steps/step

onWorkflow ->
  class window.New extends Step
    constructor: (attrs={}) ->
      super(attrs)

    button_class: () =>
      'ltext'

    default_name: () =>
      "Add new step"

    set_as_current: () =>
      workflow.sidebar_content(workflow.command_selector())
      workflow.current_step(@)

    can_add_next: () =>
      false

