#= require workflow/steps/step

onWorkflow ->
  class window.MarkAsSuccessful extends Step
    @type = 'mark_as_successful'

    constructor: (attrs) ->
      super(attrs)

    button_class: =>
      'lreport'

    default_name: =>
      'Mark as successful'

    @add_to_steps: () ->
      workflow.add_step(new MarkAsSuccessful)

    @initialize: (hash) ->
      step = new MarkAsSuccessful(hash)
      return step
