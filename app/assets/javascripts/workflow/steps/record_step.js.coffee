#= require workflow/steps/step

onWorkflow ->
  class window.Record extends Step
    @type = 'record'

    constructor: (attrs) ->
      super(attrs)

      @timeout = ko.observable (attrs.timeout || '10')
      @stop_key = ko.observable (attrs.stop_key || '#')

      @current_editing_message = ko.observable null
      @message_selectors =
        explanation:  MessageSelector.from_hash(attrs.explanation_message).with_title('Explanation').with_parent(@)
        confirmation: MessageSelector.from_hash(attrs.confirmation_message).with_title('Confirmation').with_parent(@)
      @is_editing_message = ko.computed () =>
        @current_editing_message() != null

      @is_explanation_message_invalid = ko.computed () =>
        not @message_selectors['explanation'].is_valid()

      @is_confirmation_message_invalid = ko.computed () =>
        not @message_selectors['confirmation'].is_valid()

      @is_invalid = ko.computed () =>
        @is_name_invalid() or @is_explanation_message_invalid() or @is_confirmation_message_invalid()

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
        explanation_message: @message_selectors['explanation'].to_hash()
        confirmation_message: @message_selectors['confirmation'].to_hash()
      )

    message: (msg) =>
      @message_selectors[msg]

    show_message: (msg) =>
      msg = @message_selectors[msg]
      @current_editing_message(msg)

    show_explanation_message: () =>
      @show_message('explanation')

    show_confirmation_message: () =>
      @show_message('confirmation')

    available_keys: () =>
      ['1','2','3','4','5','6','7','8','9','0','#','*']
