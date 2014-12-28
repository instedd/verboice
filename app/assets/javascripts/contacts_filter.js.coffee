class @ContactsFilter
  constructor: (filters = []) ->
    @filters = ko.observableArray(new Filter(filter) for filter in filters)
    @json = ko.computed =>
      ko.toJSON(filter.to_hash() for filter in @filters())

  addFilter: =>
    @filters.push(new Filter())

class Filter
  constructor: (attrs = {}) ->
    # @project_variable_id = ko.observable attrs.project_variable_id
    # @implicit_key = ko.observable attrs.implicit_key
    # @operator = ko.observable attrs.operator
    # @other_project_variable_id = ko.observable attrs.other_project_variable_id
    # @other_implicit_key = ko.observable attrs.other_implicit_key

    @variable = ko.observable()
    @other_variable = ko.observable()
    @value = ko.observable()

    @other_types = ['value', 'variable']
    @other_type = ko.observable()

    if attrs.project_variable_id?
      @variable(_.find(window.variables, (v) -> v.id == attrs.project_variable_id))

    if attrs.other_project_variable_id?
      @other_variable(_.find(window.variables, (v) -> v.id == attrs.other_project_variable_id))
      @other_type('variable')
    else
      @value(attrs.value)
      @other_type('value')

    @operators = [
      {text: 'is equal to', value: 'eq'},
      {text: 'greater or equal to', value: 'geq'},
      {text: 'less or equal to', value: 'leq'},
      {text: 'greater than', value: 'gt'},
      {text: 'less than', value: 'lt'},
      {text: 'is defined', value: 'defined'},
      {text: 'is undefined', value: 'undefined'},
      {text: 'contains', value: 'includes'},
    ]

    @operator = ko.observable(_.find(@operators, (op) -> op.value == attrs.operator))

  to_hash: =>
    {
      project_variable_id: @variable()?.id
      operator: @operator()?.value
      value: @value() if @other_type() == 'value'
      other_project_variable_id: @other_variable()?.id if @other_type() == 'variable'
    }


