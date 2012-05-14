onWorkflow ->
  class window.ChildStep
    constructor: (next_id, parent) ->
      @skip_step = null
      @next_id = next_id
      @parent = parent
      @is_default = false

    next: () =>
      workflow.get_step @next_id

    next_name: () =>
      if @next()? then @next().name() else "Skip to #{if @parent.next_id > 0 then @parent.next().name() else 'next step'}"

    skip: () =>
      @skip_step ?= new Skip

    to_hash: () =>
      {
        next: @next_id,
        is_default: @is_default
      }

    remove_next: () =>
      next = @next()
      if next
        @next_id = null
        next.remove_recursive()

    select_step: () =>
      return if not @next()?
      @next().set_as_current()

    child_updated: (step, new_step) =>
      @parent.child_updated(step, new_step)

    child_removed: () =>
      @parent.remove_child_step(@)

    type: () =>
      if @next()? then @next().type() else Skip.type
