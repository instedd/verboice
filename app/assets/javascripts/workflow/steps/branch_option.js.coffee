#= require workflow/steps/child_step

onWorkflow ->
  class window.BranchOption extends ChildStep
    constructor: (conditions, next_id, branch) ->
      super(next_id, branch)

      @conditions = ko.observableArray(new BranchCondition(cond) for cond in (conditions || []))
      @is_empty = ko.computed () =>
        @conditions().length == 0

      @current_editing_condition = ko.observable null
      @is_editing_condition = ko.computed () =>
        @current_editing_condition() != null

    to_hash: () =>
      $.extend(super,
        conditions: (condition.to_hash() for condition in @conditions())
      )

    add_condition: () =>
      condition = new BranchCondition({})
      condition.after_initialize()
      @conditions.push(condition)
      @current_editing_condition(condition)

    remove_condition: (condition) =>
      @conditions.remove condition
      @current_editing_condition(null) if @current_editing_condition() == condition

    show_condition: (condition) =>
      @current_editing_condition(condition)

    close_condition_edition: () =>
      @current_editing_condition(null)

    begin_edition: () =>
      @conditions_tmp = @conditions().slice(0)

    save: () =>
      @conditions_tmp = null
      @exit()

    cancel: () =>
      @conditions(@conditions_tmp)
      @exit()

    exit: () =>
      @close_condition_edition()
      @parent.current_editing_option(null)

    after_initialize: () =>
      condition.after_initialize() for condition in @conditions()

    on_step_removed: (step) =>
      condition.on_step_removed(step) for condition in @conditions()
