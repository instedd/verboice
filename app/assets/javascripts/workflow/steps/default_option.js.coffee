#= require workflow/steps/child_step

onWorkflow ->
  class window.DefaultOption extends ChildStep
    constructor: (next_id, parent) ->
      super(next_id, parent)
      @is_default = true