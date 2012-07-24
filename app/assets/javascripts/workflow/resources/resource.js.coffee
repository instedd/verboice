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
      @localized_resources = ko.observableArray (new window[h.type](h) for h in hash.localized_resources or [])

    to_hash: () =>
      id: @id()
      project_id: @project_id
      resource:
        name: @name()
        localized_resources_attributes: @pack_localized_resources()

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

    pack_localized_resources: () =>
      result = {}
      for lr, i in @localized_resources()
        do (lr, i) ->
          result[i] = lr.to_hash()
      result
