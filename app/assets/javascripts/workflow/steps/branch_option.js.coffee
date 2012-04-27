onWorkflow ->
  class window.BranchOption
    constructor: (conditions, next_id, branch) ->
      conditions = (new BranchCondition(cond.step, cond.operator, cond.value) for cond in (conditions || []))

      @conditions = ko.observableArray(conditions)
      @next_id = next_id
      @branch = branch

    next: () =>
      workflow.get_step @next_id

    next_name: () =>
      @next()?.name()

    to_hash: () =>
      conditions: (condition.to_hash() for condition in @conditions())
      next: @next_id

    remove_next: () =>
      next = @next()
      if next
        @next_id = null
        next.remove_recursive()

    select_step: () =>
      return if not @next()?
      @next().set_as_current()

    child_removed: () =>
      @branch.remove_option(@)

    add_condition: () =>
      @conditions.push(new BranchCondition(null, null, null))
