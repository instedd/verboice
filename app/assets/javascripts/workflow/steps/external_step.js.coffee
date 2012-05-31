onWorkflow ->
  class window.External extends Step
    @type = 'external'

    @classes: () ->
      (class extends External
        @external_type = external_step.name
        @display_name = external_step.display_name
        @type = "external_#{external_step.name}"
        @icon = external_step.icon) for external_step in external_steps

    constructor: (attrs) ->
      super(attrs)

    button_class: () =>
      'ltext'

    @add_to_steps: () ->
      workflow.add_step(new window[@type])

    @initialize: (hash) ->
      return new window[@type](hash)

    to_hash: () =>
      $.extend(super,
        foo: 0
      )

    default_name: () =>
      @.constructor.display_name

    display_template_id: () =>
      'external_step_template'

    icon_url: () =>
      @.constructor.icon
