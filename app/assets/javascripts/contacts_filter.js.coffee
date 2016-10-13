class @ContactsFilter
  constructor: (filters = [], count=null) ->
    @count = ko.observable(count)
    @filters = ko.observableArray(new Filter(filter) for filter in filters)
    @countDescription = ko.computed =>
      if @count()? then "(#{if @count() == 0 then 'no' else @count()} #{if @count() == 1 then 'contact' else 'contacts'} found)" else ""
    @json = ko.computed =>
      ko.toJSON(filter.to_hash() for filter in @filters())
    @variablesAndFields = window.fields.concat(window.variables);

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

    if variable_id = attrs.project_variable_id or attrs.implicit_key
      @variable(_.find(window.variables, (v) -> v.id == variable_id))
    else if attrs.field_name
      @variable(_.find(window.fields, (v) -> v.id == attrs.field_name))

    if other_variable_id = attrs.other_project_variable_id or attrs.other_implicit_key
      @other_variable(_.find(window.variables, (v) -> v.id == other_variable_id))
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
    hash = { operator: @operator()?.value }

    if @variable()?.implicit
      hash.implicit_key = @variable().id
    else if @variable()?.field
      hash.field_name = @variable().id
    else if @variable()?.id?
      hash.project_variable_id = @variable().id

    unless @operator()?.unary
      if @other_type() == 'value'
        hash.value = @value()
      else if @other_type() == 'variable'
        if @other_variable()?.implicit
          hash.other_implicit_key = @other_variable().id
        else if @other_variable()?.id?
          hash.other_project_variable_id = @other_variable().id

    return hash

  expand: =>
    @expanded(true)

  close: =>
    @expanded(false)
