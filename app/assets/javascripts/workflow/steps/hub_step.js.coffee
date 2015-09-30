onWorkflow ->
  class window.Hub extends Step
    @type = 'hub'

    constructor: (attrs) ->
      super(attrs)

      @next_id = attrs.next
      @action_label = ko.observable(attrs.action_label)
      @action_path = ko.observable(attrs.action_path)
      @bindings = ko.observableArray(Hub.recreate_bindings(attrs.bindings))

    button_class: () =>
      'lpgear'

    @add_to_steps: () ->
      workflow.add_step(new Hub)

    @initialize: (hash) ->
      step = new Hub(hash)
      return step

    to_hash: () =>
      $.extend(super, {
        action_label: @action_label(),
        action_path: @action_path(),
        bindings: _.map(@bindings(), (b) -> b.to_hash())
      })

    default_name: () =>
      'External Action'

    choose_hub_action: =>
      hubApi = new HubApi(window.hub_url)
      hubApi.openPicker('action')
        .then((path, selection) =>
          @action_path(path)
          @action_label(@compute_label(selection));
          $.get "/hub/reflect/#{path}", (data) =>
            @bindings(Hub.build_bindings(data.args))
        )

    compute_label: (selection) =>
      str = ""
      for item in selection.parents
        str += "/" unless str.length == 0
        str += item.label
      str

    @build_bindings: (args) =>
      bindings = []
      for name, properties of args
        bindings.push Hub.build_binding(name, properties)
      bindings

    @build_binding: (name, properties) =>
      label = properties.label || name

      if properties.type?.kind == "struct"
        value = null
        bindings = ko.observableArray(Hub.build_bindings(properties.type.members))
      else
        value = new window.InputSetting(value: "")
        bindings = null

      new Binding(name, label, value, bindings)

    @recreate_bindings: (bindings) ->
      _.map((bindings || []), (b) -> Hub.recreate_binding(b))

    @recreate_binding: (binding) ->
      name = binding.name
      label = binding.label

      if binding.value
        value = new window.InputSetting(binding.value)
      else
        value = null

      if binding.bindings
        bindings = ko.observableArray(Hub.recreate_bindings(binding.bindings))
      else
        bindings = null

      new Binding(name, label, value, bindings)

    class Binding
      constructor: (@name, @label, @value, @bindings) ->
        if @label.length > 0
          @label = @label.charAt(0).toUpperCase() + @label.substring(1)

      to_hash: ->
        {
          name: @name,
          label: @label,
          value: @value?.to_hash(),
          bindings: _.map((@bindings?() || []), (m) -> m.to_hash())
        }

