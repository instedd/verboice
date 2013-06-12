#= require workflow/steps/step
#= require workflow/steps/nuntium_recipient

onWorkflow ->
  class window.Nuntium extends Step
    @type = 'nuntium'

    constructor: (attrs) ->
      super(attrs)

      @next_id = attrs.next

      @resource =  new ResourceEditor(@, attrs.resource)
      @current_editing_resource = ko.observable(null)
      @is_editing_resource = ko.computed () =>
        @current_editing_resource()?

      @is_resource_invalid = ko.computed () =>
        not @resource.is_valid() or not @resource.is_text()

      @recipient = new NuntiumRecipient(attrs.recipient || { caller: true })

      @is_invalid = ko.computed () =>
        @is_name_invalid() or @is_resource_invalid() or @recipient.is_invalid()

    button_class: () =>
      'lmessage'

    default_name: () =>
      'Send SMS'

    @add_to_steps: () ->
      workflow.add_step(new Nuntium)

    @initialize: (hash) ->
      step = new Nuntium(hash)
      return step

    to_hash: () =>
      $.extend(super,
        resource: @resource.to_hash(),
        recipient: @recipient.to_hash()
      )

    show_resource: () =>
      @current_editing_resource(@resource)

