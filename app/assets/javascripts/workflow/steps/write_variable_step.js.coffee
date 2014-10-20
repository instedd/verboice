#= require workflow/steps/step

onWorkflow ->
  class window.WriteVariable extends Step
    @type = 'write_variable'

    constructor: (attrs) ->
      super(attrs)

      @variable = ko.observable attrs.variable
      @value = ko.observable attrs.value

      @is_variable_invalid = ko.computed () =>
        not @variable()

      @is_invalid = ko.computed () =>
        @is_name_invalid() or @is_variable_invalid()

    button_class: =>
      'laddressbook'

    default_name: =>
      'Write variable'

    to_hash: () =>
      $.extend(super,
        variable: @variable()
        value: @value()
      )

    @add_to_steps: () ->
      workflow.add_step(new WriteVariable)

    @initialize: (hash) ->
      step = new WriteVariable(hash)
      return step
