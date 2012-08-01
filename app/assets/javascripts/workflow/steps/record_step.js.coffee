#= require workflow/steps/step

onWorkflow ->
  class window.Record extends Step
    @type = 'record'

    constructor: (attrs) ->
      super(attrs)

      @timeout = ko.observable (attrs.timeout || '10')
      @stop_key = ko.observable (attrs.stop_key || '#')

      @current_editing_resource = ko.observable null
      @resources =
        explanation:  new ResourceEditor(@, attrs.explanation_resource)
        confirmation: new ResourceEditor(@, attrs.confirmation_resource)
      @is_editing_resource = ko.computed () =>
        @current_editing_resource() != null

      @is_explanation_resource_invalid = ko.computed () =>
        not @resources.explanation.is_valid()

      @is_confirmation_resource_invalid = ko.computed () =>
        not @resources.confirmation.is_valid()

      @is_invalid = ko.computed () =>
        @is_name_invalid() or @is_explanation_resource_invalid() or @is_confirmation_resource_invalid()

    button_class: () =>
      'lmicrophone'

    @add_to_steps: () ->
      workflow.add_step(new Record)

    @initialize: (hash) ->
      step = new Record(hash)
      return step

    to_hash: () =>
      $.extend(super,
        timeout: @timeout()
        stop_key: @stop_key()
        explanation_resource: @resources.explanation.to_hash()
        confirmation_resource: @resources.confirmation.to_hash()
      )

    resource: (res) =>
      @resources[res]

    show_resource: (res) =>
      resource = @resources[res]
      @current_editing_resource(resource)

    show_explanation_resource: () =>
      @show_resource('explanation')

    show_confirmation_resource: () =>
      @show_resource('confirmation')

    available_keys: () =>
      ['1','2','3','4','5','6','7','8','9','0','#','*']
