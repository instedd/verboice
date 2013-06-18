onWorkflow ->
  class window.NuntiumRecipient extends window.InputSetting
    constructor: (attrs) ->
      super(attrs)

      @content_kind = ko.observable (if attrs.caller
          'caller'
        else if attrs.variable? and attrs.variable != ''
          'variable'
        else if attrs.step? and attrs.step != ''
          'step'
        else if attrs.response? and attrs.response != ''
          'response'
        else
          'value')

      @is_invalid = ko.computed () =>
        if @content_kind() == 'caller'
          false
        else if @content_kind() == 'variable'
          not @variable()
        else if @content_kind() == 'step'
          not @step_id()
        else if @content_kind() == 'response'
          not @response()
        else
          not @value()

    content_kinds: () =>
      return [{text: 'Caller', value: 'caller'},
              {text: 'Variable', value: 'variable'},
              {text: 'Step', value: 'step'},
              {text: 'Response', value: 'response'},
              {text: 'Value', value: 'value'}]

    to_hash: () =>
      $.extend super(), {
        caller: if @content_kind() == 'caller' then true else false
      }

    available_steps: () =>
      {name: step.name(), value: step.id} for step in workflow.steps() when (step.type() == 'capture')

