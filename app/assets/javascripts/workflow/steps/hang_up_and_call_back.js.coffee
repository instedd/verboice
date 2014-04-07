#= require workflow/steps/step

onWorkflow ->
  class window.HangUpAndCallBack extends Step
    @type = 'hang_up_and_call_back'

    constructor: (attrs) ->
      super(attrs)
      @dial_prefix = ko.observable attrs.dial_prefix
      @when = ko.observable(attrs.when ? 'immediately')
      @delay = ko.observable(attrs.delay ? '1 hour')

      @is_delay_invalid = ko.computed =>
        @when() == 'later' && !@delay().match(/^\s*\d+\s*(se?c?o?n?d?s?|mi?n?u?t?e?s?|ho?u?r?s?|da?y?s?)\s*$/)
      @is_invalid = ko.computed =>
        @is_name_invalid() || @is_delay_invalid()

    button_class: =>
      'lcallback'

    default_name: =>
      'Call back'

    to_hash: () =>
      $.extend(super,
        dial_prefix: @dial_prefix(),
        when: @when(),
        delay: @delay(),
      )

    @add_to_steps: () ->
      workflow.add_step(new HangUpAndCallBack)

    @initialize: (hash) ->
      step = new HangUpAndCallBack(hash)
      return step
