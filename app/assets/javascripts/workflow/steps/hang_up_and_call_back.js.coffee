#= require workflow/steps/step

onWorkflow ->
  class window.HangUpAndCallBack extends Step
    @type = 'hang_up_and_call_back'

    constructor: (attrs) ->
      super(attrs)
      @dial_prefix = ko.observable attrs.dial_prefix
      @when = ko.observable(attrs.when ? 'immediately')
      @delay = ko.observable(attrs.delay ? '1h')

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
