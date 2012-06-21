#= require workflow/steps/external_step_setting
#= require workflow/steps/external_step_response

onWorkflow ->
  class window.External extends Step
    @type = 'external'

    @classes: () ->
      (class extends External
        @external_step_type = external_step.name
        @external_step_type_guid = external_step.guid
        @display_name = external_step.display_name
        @variables = external_step.variables
        @response_variables = external_step.response_variables
        @type = "external_#{external_step.guid}"
        @icon = external_step.icon) for external_step in external_steps

    constructor: (attrs) ->
      super(attrs)

      # Setup settings
      attrs.settings ?= []
      settings = []
      for variable in @variables()
        setting = ((s for s in attrs.settings when s.name == variable.name)[0] or {})
        setting = $.extend({name: variable.name, display_name: variable.display_name}, setting)
        settings.push(new ExternalStepSetting(@, setting))
      @settings = ko.observableArray(settings)

      # Setup responses
      attrs.responses ?= []
      responses = []
      for variable in @response_variables()
        response = ((s for s in attrs.responses when s.name == variable.name)[0] or {})
        response = $.extend({name: variable.name, display_name: variable.display_name}, response)
        responses.push(new ExternalStepResponse(response))
      @responses = ko.observableArray(responses)

      @current_editing_setting = ko.observable null
      @is_editing_setting = ko.computed () =>
        @current_editing_setting() != null

    button_class: () =>
      if @is_icon_external()
        'lpgear'
      else
        'lp' + @icon_url()

    @add_to_steps: () ->
      workflow.add_step(new window[@type])

    @initialize: (hash) ->
      return new window[@type](hash)

    @can_handle: (hash) ->
      hash_type = if hash.external_step_guid? then "#{hash.type}_#{hash.external_step_guid}" else hash.type
      return hash_type == @type

    @is_external: () ->
      true

    to_hash: () =>
      $.extend(super,
        external_step_guid: @.constructor.external_step_type_guid
        type: 'external'
        settings: (setting.to_hash() for setting in @settings())
        responses: (response.to_hash() for response in @responses())
      )

    default_name: () =>
      @.constructor.display_name

    display_template_id: () =>
      'external_step_template'

    defines_store: () =>
      _.any(response.defines_store() for response in @responses())

    store: () =>
      (response.store() for response in @responses())

    icon_url: () =>
      @.constructor.icon

    is_icon_external: () =>
      @.constructor.icon.indexOf('http') == 0

    variables: () =>
      @.constructor.variables

    response_variables: () =>
      @.constructor.response_variables

    on_step_removed: (step) =>
      setting.on_step_removed(step) for setting in @settings()

    edit_setting: (setting) =>
      setting.on_begin_edition()
      @current_editing_setting(setting)
