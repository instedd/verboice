onWorkflow ->
  class window.ExternalStepResponse

    constructor: (attrs) ->
      @name = attrs.name
      @display_name = attrs.display_name
      @defines_store = ko.observable attrs.variable?
      @store = ko.observable attrs.variable

    to_hash: () =>
      name: @name
      variable: if @defines_store() then @store() else null

    available_variables: () =>
      workflow.all_variables().sort()