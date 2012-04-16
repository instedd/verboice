onWorkflow ->
  class window.Step
    constructor: (attrs) ->
      @root = false
      @id = attrs['id']
      @root = attrs['root']
      @name = ko.observable(attrs['name'] || @default_name())

    @from_hash: (hash) ->
      for step_type in step_types
        if hash.type.toLowerCase() == step_type.toLowerCase()
          return window[step_type].from_hash(hash)
      throw "Command type not recognised #{hash['type']}"

    to_hash: () =>
      id: @id
      name: @name()
      type: @.constructor.name.toLowerCase()
      root: @root

    parent: () =>
      workflow.get_parent(@)

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

    children: () =>
      (step for step in workflow.steps() when step.id in @next_ids())

    child_removed: (child) =>
      null

    item_template_id: () =>
      'workflow_step_template'

    display_template_id: () =>
      "#{@default_name().toLowerCase()}_step_template"

    default_name: () =>
      @.constructor.name

