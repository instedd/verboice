#= require workflow/steps/step

onWorkflow ->
  class window.Transfer extends Step
    @type = 'transfer'

    constructor: (attrs) ->
      super(attrs)

      @next_id = attrs.next
      @address = ko.observable attrs.address
      @channel = ko.observable attrs.channel

    button_class: () =>
      'lforwardcall'

    @add_to_steps: () ->
      workflow.add_step(new Transfer)

    @initialize: (hash) ->
      step = new Transfer(hash)
      return step

    to_hash: () =>
      $.extend(super,
        address: @address()
        channel: @channel()
      )

    default_name: () =>
      'Forward call'
