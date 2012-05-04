#= require workflow/steps/child_step

onWorkflow ->
  class window.BranchOption extends ChildStep
    constructor: (conditions, next_id, branch) ->
      super(next_id, branch)

      @conditions = ko.observableArray(new BranchCondition(cond) for cond in (conditions || []))
      @is_else = false
      @is_empty = ko.computed () =>
        @conditions().length == 0

    to_hash: () =>
      $.extend(super,
        conditions: (condition.to_hash() for condition in @conditions())
        is_else: @is_else
      )

    add_condition: () =>
      @conditions.push(new BranchCondition({}))

  class window.BranchElseOption extends BranchOption
    constructor: (next_id, branch) ->
      super([], next_id, branch)
      @is_else = true
