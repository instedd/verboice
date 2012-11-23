#= require workflow/steps/step

onWorkflow ->
  class window.MarkAsFailed extends Step
    @type = 'mark_as_failed'

    constructor: (attrs) ->
      super(attrs)

    button_class: =>
      'lunknown'

    default_name: =>
      'Mark as failed'

    @add_to_steps: () ->
      workflow.add_step(new MarkAsFailed)

    @initialize: (hash) ->
      step = new MarkAsFailed(hash)
      return step
