onWorkflow ->
  class window.BranchCondition
    constructor: (attrs) ->

      # Left hand side
      @variable = ko.observable attrs.variable
      @step_id = ko.observable attrs.step
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

      # Operator
      @operator = ko.observable attrs.operator
      @operators = [
        {text: 'is equal to', value: '=='},
        {text: 'greater or equal to', value: '>='},
        {text: 'less or equal to', value: '<='},
        {text: 'greater than', value: '>'},
        {text: 'less than', value: '<'}
      ]


      # Right hand side
      @rhs_value = ko.observable(attrs.rhs_value || attrs.value) # Legacy name
      @rhs_variable = ko.observable attrs.rhs_variable
      @rhs_kind = ko.observable(if attrs.rhs_variable? and attrs.rhs_variable != '' then 'variable' else 'value')


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
      rhs_value: if @rhs_kind() == 'value' then @rhs_value() else null
      rhs_variable: if @rhs_kind() == 'variable' then @rhs_variable() else null

    variable_or_step_name: () =>
      if @variable()
        @variable()
      else if @step_id()
        workflow.get_step(parseInt(@step_id())).name()
      else
        ''

    operator_text_for: (operator_value) =>
      if operator_value
        (operator.text for operator in @operators when operator.value == operator_value)[0]
      else
        ''

    available_variables: () =>
      workflow.all_variables().sort()

    available_conditions: () =>
      (
        {name: variable, group: 'Variables', value: "var-#{variable}"} for variable in workflow.variables().sort()
      ).concat(
        {name: variable, group: 'Variables', value: "var-#{variable}"} for variable in distinct_variables.sort() when variable not in workflow.variables()
      ).concat(
        {name: step.name(), group: "#{step.default_name()}s", value: "stp-#{step.id}"} for step in workflow.steps() when (step.type() == 'capture') || (step.type() == 'menu')
      )

    after_initialize: () =>
      @description = ko.computed () =>
        "#{@variable_or_step_name()} #{@operator_text_for(@operator())} #{@rhs()}"

    rhs: () =>
      (if @rhs_kind() == 'value' then @rhs_value() else @rhs_variable()) or ''

    on_step_removed: (step) =>
      @step_id(null) if step.id == parseInt(@step_id())
