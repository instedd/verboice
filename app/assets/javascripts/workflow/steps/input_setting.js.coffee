onWorkflow ->
  class window.InputSetting
    constructor: ({value: value, variable: variable, response: response, step: step}) ->

      @value = ko.observable value
      @variable = ko.observable variable
      @response = ko.observable response
      @step_id = ko.observable step

      @content_kind = ko.observable (if variable? and variable != ''
          'variable'
        else if step? and step != ''
          'step'
        else if response? and response != ''
          'response'
        else
          'value')

    to_hash: () =>
      step: if @content_kind() == 'step' then @step_id() else null
      variable: if @content_kind() == 'variable' then @variable() else null
      value: if @content_kind() == 'value' then @value() else null
      response: if @content_kind() == 'response' then @response() else null

    content_kinds: () =>
      return [{text: 'Variable', value: 'variable'},
      {text: 'Step', value: 'step'},
      {text: 'Response', value: 'response'},
      {text: 'Value', value: 'value'}]

    available_variables: () =>
      workflow.all_variables().sort()

    available_steps: () =>
      {name: step.name(), value: step.id} for step in workflow.steps() when (step.type() == 'capture') || (step.type() == 'menu')

    available_responses: () =>
      _.flatten({name: "#{step.name()} - #{variable.display_name}", value: "#{step.id}_#{variable.name}"} for variable in step.response_variables() for step in workflow.steps() when step.response_variables?)

    on_step_removed: (step) =>
      @step_id(null) if step.id == parseInt(@step_id())
      @response(null) if step.id == parseInt(@response()) # Note that parseInt("123_resp") == "123"

    description: () =>
      if @content_kind() == 'step'
        workflow.get_step(@step_id()).name()
      else if @content_kind() == 'variable'
        @variable()
      else if @content_kind() == 'value'
        @value()
      else if @content_kind() == 'response'
        (response.name for response in @available_responses() when response.value == @response())[0]
