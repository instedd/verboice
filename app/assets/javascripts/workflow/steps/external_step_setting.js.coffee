onWorkflow ->
  class window.ExternalStepSetting
    constructor: (parent, attrs) ->

      @name = attrs.name
      @display_name = attrs.display_name

      @value = ko.observable attrs.value
      @variable = ko.observable attrs.variable
      @response = ko.observable attrs.response
      @step_id = ko.observable attrs.step

      @content_kind = ko.observable (if attrs.variable? and attrs.variable != ''
          'variable'
        else if attrs.step? and attrs.step != ''
          'step'
        else if attrs.response? and attrs.response != ''
          'response'
        else
          'value')

      @parent = parent

    to_hash: () =>
      name: @name
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

    on_begin_edition: () =>
      @content_kind_tmp = @content_kind()
      @value_tmp = @value()
      @variable_tmp = @variable()
      @step_id_tmp = @step_id()

    save: () =>
      @exit()

    cancel: () =>
      @content_kind(@content_kind_tmp)
      @value(@value_tmp)
      @variable(@variable_tmp)
      @step_id(@step_id_tmp)
      @exit()

    exit: () =>
      @parent.current_editing_setting(null)
