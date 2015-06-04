class @ContactsFilter
  constructor: (filters = [], count=null) ->
    @count = ko.observable(count)
    @filters = ko.observableArray(new Filter(filter) for filter in filters)
    @countDescription = ko.computed =>
      if @count()? then "(#{if @count() == 0 then 'no' else @count()} #{if @count() == 1 then 'contact' else 'contacts'} found)" else ""
    @json = ko.computed =>
      ko.toJSON(filter.to_hash() for filter in @filters())

  addFilter: =>
    filter = new Filter()
    filter.expand()
    @filters.push(filter)

  removeFilter: (filter) =>
    @filters.remove(filter)

  setCount: (value) =>
    @count(value)

class Filter
  constructor: (attrs = {}) ->
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
      @value(attrs.value || '')
      @other_type('value')

    @operators = [
      {text: 'is equal to', value: 'eq'},
      {text: 'greater or equal to', value: 'geq'},
      {text: 'less or equal to', value: 'leq'},
      {text: 'greater than', value: 'gt'},
      {text: 'less than', value: 'lt'},
      {text: 'contains', value: 'includes'},
      {text: 'is defined', value: 'defined', unary: true},
      {text: 'is undefined', value: 'undefined', unary: true}
    ]

    @operator = ko.observable(_.find(@operators, (op) -> op.value == attrs.operator))

    @expanded = ko.observable false
    @unary = ko.computed =>
      @operator()?.unary

    @description = ko.computed =>
      if @unary()
        "#{@variable()?.name} #{@operator()?.text}"
      else
        right = if @other_type() == 'value' then @value() else @other_variable()?.name
        "#{@variable()?.name} #{@operator()?.text} #{right}"

  to_hash: =>
    {
      project_variable_id: @variable()?.id
      operator: @operator()?.value
      value: @value() if @other_type() == 'value'
      other_project_variable_id: @other_variable()?.id if @other_type() == 'variable'
    }

  expand: =>
    @expanded(true)

  close: =>
    @expanded(false)


