onWorkflow ->
  class window.BranchCondition
    constructor: (attrs) ->
      @variable = attrs.variable
      @step_id = attrs.step

      @operator = ko.observable attrs.operator
      @value = ko.observable attrs.value

      @subject = ko.computed
        read: () =>
          if @variable
            "var-#{@variable}"
          else if @step_id
            "stp-#{@step_id}"
          else
            null
        write: (val) =>
          if !val
            @step_id = null
            @variable = null
          else
            kind = val[0..2]
            val = val[4..-1]
            if kind == 'var'
              @variable = val
              @step_id = null
            else if kind == 'stp'
              @step_id = val
              @variable = null
            else
              throw "Invalid option kind #{kind}"

    to_hash: () =>
      step: @step_id
      variable: @variable
      operator: @operator()
      value: @value()

    available_conditions: () =>
      ({name: variable, group: 'Variables', value: "var-#{variable}"} for variable in distinct_variables).concat(
        {name: step.name(), group: step.default_name(), value: "stp-#{step.id}"} for step in workflow.steps() when (step.type() == 'capture') || (step.type() == 'menu')
      )

