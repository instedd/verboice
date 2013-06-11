#= require workflow/steps/step

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

      @rcpt_options = [{ value: 'caller',   display: 'Caller' },
                       { value: '3rdparty', display: '3rd. party' },
                       { value: 'variable', display: 'From variable' }]

      @rcpt_type = ko.observable(attrs.rcpt_type || 'caller')
      @rcpt_phone_number = ko.observable(attrs.rcpt_phone_number)
      @rcpt_variable = ko.observable(attrs.rcpt_variable)

      @is_rcpt_phone_invalid = ko.computed () =>
        not @rcpt_phone_number()
      @is_rcpt_variable_invalid = ko.computed () =>
        not @rcpt_variable()

      @is_rcpt_invalid = ko.computed () =>
        (@rcpt_type() == '3rdparty' and @is_rcpt_phone_invalid()) or
          (@rcpt_type() == 'variable' and @is_rcpt_variable_invalid())

      @is_invalid = ko.computed () =>
        @is_name_invalid() or @is_resource_invalid() or @is_rcpt_invalid()

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
        rcpt_type: @rcpt_type(),
        rcpt_phone_number: @rcpt_phone_number(),
        rcpt_variable: @rcpt_variable()
      )

    show_resource: () =>
      @current_editing_resource(@resource)

    available_variables: () =>
      workflow.all_variables().sort()

