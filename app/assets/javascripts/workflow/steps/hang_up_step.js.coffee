#= require workflow/steps/step

onWorkflow ->
  class window.HangUp extends Step
    @type = 'hang_up'

    constructor: (attrs) ->
      super(attrs)

    button_class: () =>
      'lphone'

    @add_to_steps: () ->
      workflow.add_step(new Play)

    @initialize: (hash) ->
      new HangUp(hash)

    default_name: () =>
      'Hang Up'

    can_insert_after: () =>
      false

    can_add_next: () =>
      false

    can_continue: () =>
      false