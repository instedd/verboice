window.initChannelSettings = (newChannel, servers) ->
  class Server
    constructor: (data) ->
      @host = ko.observable data?.host
      @register = ko.observable data?.register
      @direction = ko.observable(data?.direction ? 'both')
      @expanded = ko.observable data?.expanded
      @isNew = ko.observable data?.isNew
      @editing = ko.observable data?.editing
      @hasfocus = ko.observable data?.hasfocus
      @description = ko.computed =>
        str = if @register() then 'Register' else 'No register'
        str += ', '
        str += if @direction() == 'inbound' then 'incoming' else (if @direction() == 'outbound' then 'outgoing' else 'both directions')
        str
      @saveText = ko.computed => if @isNew() then 'Add' else 'Update'
      @valid = ko.computed => $.trim(@host()).length > 0

    edit: =>
      @originalHost = @host()
      @originalRegister = @register()
      @originalDirection = @direction()
      @expanded(true)

    save: =>
      @isNew(false)
      @expanded(false)

    cancel: =>
      @host(@originalHost)
      @register(@originalRegister)
      @direction(@originalDirection)
      @expanded(false)

  class ChannelSettingsViewModel
    constructor: (newChannel, servers) ->
      @servers = ko.observableArray _.map(servers, (x) ->
        server = new Server(x)
        server.expanded(newChannel)
        server.isNew(newChannel)
        server.editing(newChannel)
        server
        )
      @editingServer = ko.observable(newChannel)

    addServer: =>
      @servers.push new Server(expanded: true, direction: 'both', isNew: true, editing: true, hasfocus: true)
      @editingServer(true)

    editServer: (server) =>
      server.editing(true)
      server.edit()
      @editingServer(true)

    saveServer: (server) =>
      server.editing(false)
      server.save()
      @editingServer(false)

    cancelServer: (server) =>
      if server.isNew()
        @servers.remove server
      else
        server.cancel()
      server.editing(false)
      @editingServer(false)

    deleteServer: (server) =>
      if server.isNew()
        @servers.remove server
      else
        if confirm("Are you sure you want to delete this server?")
          @servers.remove server

      if server.editing()
        @editingServer(false)


  ko.applyBindings new ChannelSettingsViewModel(newChannel, servers)
