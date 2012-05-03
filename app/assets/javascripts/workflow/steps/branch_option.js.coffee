#= require workflow/steps/child_step

onWorkflow ->
  class window.BranchOption extends ChildStep
    constructor: (conditions, next_id, branch) ->
      super(next_id, branch)

      conditions = (new BranchCondition(cond.step, cond.operator, cond.value) for cond in (conditions || []))
      @conditions = ko.observableArray(conditions)
      @is_else = false
      @is_empty = ko.computed () =>
        @conditions().length == 0

    to_hash: () =>
      $.extend(super,
        conditions: (condition.to_hash() for condition in @conditions())
        is_else: @is_else
      )

    add_condition: () =>
      @conditions.push(new BranchCondition(null, null, null))

  class window.BranchElseOption extends BranchOption
    constructor: (next_id, branch) ->
      super([], next_id, branch)
      @is_else = true
