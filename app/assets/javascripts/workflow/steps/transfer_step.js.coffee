#= require workflow/steps/step

onWorkflow ->
  class window.Transfer extends Step
    @type = 'transfer'

    constructor: (attrs) ->
      super(attrs)

      @next_id = attrs.next
      @address = ko.observable attrs.address
      @channel = ko.observable attrs.channel
      
      @successful_after_check = ko.observable(_.isString(attrs.successful_after))
      @successful_after_input = ko.observable(attrs.successful_after or "")
      @successful_after = ko.computed () => if @successful_after_check() then @successful_after_input() else null

      @is_address_invalid = ko.computed () =>
        not @address()

      @is_successful_after_invalid = ko.computed () =>
        @successful_after_check() and not @successful_after_input().match(/^\s*\d+\s*(s|ms?|hs?|seconds?|minutes?|hours?)\s*$/i)

      @is_invalid = ko.computed () =>
        @is_name_invalid() or @is_address_invalid() or @is_successful_after_invalid()

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
        successful_after: @successful_after()
      )

    default_name: () =>
      'Forward Call'