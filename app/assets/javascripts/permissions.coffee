window.initPermissions = (params) ->
  class ViewModel
    constructor: ->
      @users = ko.observableArray _.map params.users, (user) ->
        new User(user)

    addUser: =>
      $.post "/permissions/add_account", email: $("#add-user").val(), (data) =>
        if data.ok
          @users.push(new User(data.account))
          $("#add-user").val('')
        else
          alert "Error: #{data.error}"

  class User
    constructor: (data) ->
      @id = data.id
      @email = data.email

      @projects = ko.observableArray _.map params.projects, (project) =>
        new Project(@, project)

      @channels = ko.observableArray _.map params.channels, (channel) =>
        new Channel(@, channel)

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

  ko.applyBindings(new ViewModel)

  $('#add-user').autocomplete source: (request, response) ->
    $.get "/permissions/autocomplete", {term: request.term}, response
