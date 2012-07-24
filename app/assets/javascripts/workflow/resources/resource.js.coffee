onWorkflow ->
  class window.Resource

    @find: (id, callback) ->
      $.getJSON "/projects/#{project_id}/resources/#{id}.json", (data) ->
        callback?(new Resource(data))

    @search: (q, callback) ->
      $.getJSON "/projects/#{project_id}/resources.json?q=#{q}", (data) ->
        callback?((new Resource(i) for i in data))

    constructor: (hash = {}) ->
      @name = ko.observable hash.name
      @id = ko.observable hash.id
      @project_id = hash.project_id || project_id

    to_hash: () =>
      id: @id()
      project_id: @project_id
      resource:
        name: @name()

    save: (callback) =>
      data = @to_hash()
      if @id()
        data._method = 'put'
        $.post "/projects/#{@project_id}/resources/#{@id()}.json", data, (response) =>
          callback?(@)
      else
        $.post "/projects/#{@project_id}/resources.json", data, (response) =>
          @id(response.id)
          callback?(@)
