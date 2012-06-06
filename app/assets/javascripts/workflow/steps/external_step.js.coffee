#= require workflow/steps/external_step_setting

onWorkflow ->
  class window.External extends Step
    @type = 'external'

    @classes: () ->
      (class extends External
        @external_service_id = external_step.external_service_id
        @external_step_type = external_step.name
        @external_step_type_id = external_step.id
        @display_name = external_step.display_name
        @variables = external_step.variables
        @type = "external_#{external_step.id}"
        @icon = external_step.icon) for external_step in external_steps

    constructor: (attrs) ->
      super(attrs)

      attrs.settings ?= []
      settings = []
      for variable in @variables()
        setting = ((s for s in attrs.settings when s.name == variable.name)[0] or {})
        setting = $.extend({name: variable.name, display_name: variable.display_name}, setting)
        settings.push(new ExternalStepSetting(setting))
      @settings = ko.observableArray(settings)

    button_class: () =>
      if @is_icon_external()
        'lpgear'
      else
        'lp' + @icon_url()

    @add_to_steps: () ->
      workflow.add_step(new window[@type])

    @initialize: (hash) ->
      return new window[@type](hash)

    to_hash: () =>
      $.extend(super,
        external_step_id: @.constructor.external_step_type_id
        type: 'external'
        settings: (setting.to_hash() for setting in @settings())
      )

    default_name: () =>
      @.constructor.display_name

    display_template_id: () =>
      'external_step_template'

    icon_url: () =>
      @.constructor.icon

    is_icon_external: () =>
      @.constructor.icon.indexOf('http') == 0

    variables: () =>
      @.constructor.variables

    on_step_removed: (step) =>
      setting.on_step_removed(step) for setting in @settings()
