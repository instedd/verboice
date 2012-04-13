onWorkflow ->
  class window.Step
    constructor: (attrs) ->
      @root = false
      @id = attrs['id']
      @root = attrs['root']

    @from_hash: (hash) ->
      item = null
      switch hash['type'].toLowerCase()
        when 'menu'
          item = Menu.from_hash(hash)
        else
          throw "Command type not recognised #{hash['type']}"

      return item

    parent: () =>
      workflow.get_parent(@)

    is_current_step: () =>
      workflow.current_step == @

    remove_with_confirm: () =>
      name = @name?() || "this step"
      if confirm("Are you sure you want to remove #{name} and all steps after it?")
        @remove()

    remove: (notify=true) =>
      @parent().child_removed @ if notify
      workflow.remove_step @

    set_as_current: () =>
      workflow.set_as_current @

    children: () =>
      (step for step in workflow.steps() when step.id in @next_ids())

    item_template_id: () =>
      'workflow_step_template'

    child_removed: (child) =>
      null

  # ---------------------------------------------------------------------------
