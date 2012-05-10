onWorkflow ->
  class window.BranchCondition
    constructor: (attrs) ->
      @variable = ko.observable attrs.variable
      @step_id = ko.observable attrs.step

      @operator = ko.observable attrs.operator
      @value = ko.observable attrs.value

      @subject = ko.computed
        read: () =>
          @subject_value()
        write: (val) =>
          if !val
            @step_id(null)
            @variable(null)
          else
            kind = val[0..2]
            val = val[4..-1]
            if kind == 'var'
              @variable(val)
              @step_id(null)
            else if kind == 'stp'
              @variable(null)
              @step_id(val)
            else
              throw "Invalid option kind #{kind}"

    subject_value: () =>
      if @variable()?
        "var-#{@variable()}"
      else if @step_id()?
        "stp-#{@step_id()}"
      else
        null

    to_hash: () =>
      step: @step_id()
      variable: @variable()
      operator: @operator()
      value: @value()

    available_conditions: () =>
      (
        {name: variable, group: 'Variables', value: "var-#{variable}"} for variable in workflow.variables().sort()
      ).concat(
        {name: variable, group: 'Variables', value: "var-#{variable}"} for variable in distinct_variables.sort() when variable not in workflow.variables()
      ).concat(
        {name: step.name(), group: "#{step.default_name()}s", value: "stp-#{step.id}"} for step in workflow.steps() when (step.type() == 'capture') || (step.type() == 'menu')
      )
