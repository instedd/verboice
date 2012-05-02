onWorkflow ->
  class window.BranchOption
    constructor: (conditions, next_id, branch) ->
      conditions = (new BranchCondition(cond.step, cond.operator, cond.value) for cond in (conditions || []))

      @conditions = ko.observableArray(conditions)
      @skip_step = null
      @is_else = false
      @next_id = next_id
      @branch = branch

      @is_empty = ko.computed () =>
        @conditions().length == 0

    next: () =>
      workflow.get_step @next_id

    next_name: () =>
      if @next()? then @next().name() else "Skip to #{if @menu.next_id > 0 then @menu.next().name() else 'next step'}"

    skip: () =>
      @skip_step ?= new Skip

    to_hash: () =>
      conditions: (condition.to_hash() for condition in @conditions())
      is_else: @is_else
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

  class window.BranchElseOption extends BranchOption
    constructor: (next_id, branch) ->
      super([], next_id, branch)
      @is_else = true
