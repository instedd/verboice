onWorkflow ->
  class window.Step
    constructor: (attrs) ->
      @root = false
      @id = attrs.id || workflow.generate_id()
      @root = attrs.root
      @name = ko.observable(attrs.name || @default_name())
      @next_id = attrs.next

    @from_hash: (hash) ->
      if typeof(hash.type) == "string"
        for step_type in step_types
          if hash.type == step_type.type
            return window[step_type.type].initialize(hash)
        throw "Command type not recognised #{hash['type']}"
      else
        return hash.type.initialize(hash)

    @initialize: (hash) ->
      return new @(hash)

    to_hash: () =>
      id: @id
      name: @name()
      type: @type()
      root: @root
      next: (if @next_id > 0 then @next_id else null)

    parent: () =>
      workflow.get_parent(@)

    next: () =>
      workflow.get_step(@next_id)

    can_add_next: () =>
      not @next_id?

    is_serializable: () =>
      true

    is_current_step: () =>
      workflow.current_step == @

    remove_with_confirm: () =>
      name = @name?() || "this step"
      if confirm("Are you sure you want to remove #{name} and all steps after it?")
        @remove()

    remove: (notify=true) =>
      @parent()?.child_removed @ if notify
      workflow.remove_step @

    set_as_current: () =>
      workflow.set_as_current @

    item_template_id: () =>
      'workflow_step_template'

    display_template_id: () =>
      "#{@type()}_step_template"

    default_name: () =>
      "#{@type()[0].toUpperCase()}#{@type()[1..-1]}"

    type: () =>
      @.constructor.type
