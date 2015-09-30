window.initContactsUploadCSV = (projectID, columnSpecs, variables) ->
  class ContactsUploadCSVModel
    constructor: ->
      @columnSpecs = ko.observableArray _.map columnSpecs, (spec) -> new ColumnSpec(spec)
      @importing = ko.observable(false)
      @variables = variables
      @actions = [
        {name: 'ignore', label: 'Ignore'},
        {name: 'phone_number', label: 'Phone Number'},
        {name: 'existing_variable', label: 'Existing Variable'},
        {name: 'new_variable', label: 'New Variable'},
      ]
      @error = ko.computed =>
        phoneNumberCount = 0
        varNames = []

        for columnSpec in @columnSpecs()
          columnSpec.error("")

        for columnSpec in @columnSpecs()
          switch columnSpec.action()
            when 'phone_number'
              phoneNumberCount += 1
              if phoneNumberCount > 1
                error = "'Phone number' column specified more than once"
                columnSpec.error(error)
                return error
            when 'new_variable'
              name = $.trim(columnSpec.name())
              if name.length == 0
                error = "New variable name can't be blank"
                columnSpec.error(error)
                return error

              if _.any(@variables, (v) -> v.name == name)
                error = "A variable named '#{name}' already exists"
                columnSpec.error(error)
                return error

              if _.indexOf(varNames, name) >= 0
                error = "Variable name '#{name}' already used"
                columnSpec.error(error)
                return error

              varNames.push name

        if phoneNumberCount == 0
          return "At least one column must be used as 'Phone number'"

        ""

    importCSV: =>
      @importing(true)
      json = _.map @columnSpecs(), (spec) -> spec.toJSON()
      $.post("/projects/#{projectID}/contacts/import_csv.json", JSON.stringify(column_specs: json))
        .always (data) -> window.location = "/projects/#{projectID}/contacts"

  class ColumnSpec
    constructor: (data) ->
      @name = ko.observable(data.name)
      @action = ko.observable(data.action)
      @id = ko.observable(data.id)
      @source = ko.observable(data.source)
      @error = ko.observable("")

    toJSON: =>
      action: @action()
      name: @name()
      id: @id()
      source: @source()

  window.contactsUploadCSVModel = new ContactsUploadCSVModel()
  ko.applyBindings(window.contactsUploadCSVModel, document.getElementById('contacts-upload-csv'))
