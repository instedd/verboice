#= require workflow/steps/input_setting

onWorkflow ->
  class window.ExternalStepSetting extends window.InputSetting
    constructor: (parent, attrs) ->
      super(attrs)

      @name = attrs.name
      @display_name = attrs.display_name
      @parent = parent

    to_hash: () =>
      $.extend super(), {
        name: @name
        display_name: @display_name
      }

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
      desc = super()
      if desc? then "(#{desc})" else null

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
