window.initPermissions = (params) ->
  class ViewModel
    constructor: ->
      @users = ko.observableArray _.map params.users, (user) ->
        new User(user)

    addUser: =>
      $.post "/permissions/add_account", email: $("#add-user").val(), (data) =>
        if data.ok
          @users.push(new User(data.account, true))
          $("#add-user").val('')
        else
          alert "Error: #{data.error}"

    addUserKeydown: (model, event) =>
      if event.keyCode == 13
        @addUser()
      else
        true

  class User
    constructor: (data, expanded = false) ->
      @id = data.id
      @email = data.email
      @expanded = ko.observable(expanded)
      @projectsExpanded = ko.observable(expanded)
      @channelsExpanded = ko.observable(expanded)

      @projects = ko.observableArray _.map params.projects, (project) =>
        new Project(@, project)

      @channels = ko.observableArray _.map params.channels, (channel) =>
        new Channel(@, channel)

    toggleExpanded: =>
      @expanded(!@expanded())

    toggleProjectsExpanded: =>
      @projectsExpanded(!@projectsExpanded())

    toggleChannelsExpanded: =>
      @channelsExpanded(!@channelsExpanded())

  class PermissionObject
    constructor: (@user, data) ->
      @id = data.id
      @name = data.name

      permission = _.find params.permissions, (p) => p.type == @className() && p.model_id == @id && p.user_id == @user.id
      @role = ko.observable(permission?.role ? "none")

      @role.subscribe =>
        $.post "/permissions/update", {account_id: @user.id, type: @className(), model_id: @id, role: @role()}

  class Project extends PermissionObject
    className: => "Project"

  class Channel extends PermissionObject
    className: => "Channel"

  ko.applyBindings(new ViewModel, document.getElementById('container'))

  $('#add-user').autocomplete source: (request, response) ->
    $.get "/permissions/autocomplete", {term: request.term}, response
