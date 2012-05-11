#= require workflow/steps/step

onWorkflow ->
  class window.Skip extends Step
    @type = 'skip'

    constructor: () ->
      super({})
      @id = null

    leaves: () =>
      [@]

    next: () =>
      null

    can_insert_before: () =>
      false

    can_insert_after: () =>
      false

  window['skip'] = Skip
