#= require workflow/steps/step

onWorkflow ->
  class window.Play extends Step
    @type = 'play'

    constructor: (attrs) ->
      super(attrs)

      @next_id = attrs.next

      @resource =  new ResourceEditor(@, attrs.resource)
      @current_editing_resource = ko.observable(null)
      @is_editing_resource = ko.computed () =>
        @current_editing_resource()?

      @is_resource_invalid = ko.computed () =>
        not @resource.is_valid()

      @is_invalid = ko.computed () =>
        @is_name_invalid() or @is_resource_invalid()

    button_class: () =>
      'lsound'

    @add_to_steps: () ->
      workflow.add_step(new Play)

    @initialize: (hash) ->
      step = new Play(hash)
      return step

    to_hash: () =>
      $.extend(super,
        resource: @resource.to_hash()
      )

    show_resource: () =>
      @current_editing_resource(@resource)
