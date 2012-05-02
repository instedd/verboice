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

  window['skip'] = Skip
