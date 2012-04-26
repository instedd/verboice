onWorkflow ->
  class window.BranchCondition
    constructor: (step_id, operator, value) ->
      @step_id = ko.observable step_id
      @operator = ko.observable operator
      @value = ko.observable value

    to_hash: () =>
      step: @step_id()
      operator: @operator()
      value: @value()

    available_steps: () =>
      (step for step in workflow.steps() when (step.type() == 'capture') || (step.type() == 'menu'))
