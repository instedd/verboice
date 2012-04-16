onWorkflow ->
  class window.MenuOption
    constructor: (num, next_id, menu) ->
      @number = ko.observable num
      @next_id = next_id
      @menu = menu
      @available_numbers = ko.computed () =>
        @menu.available_numbers().concat([@number()]).sort()

    next: () =>
      workflow.get_step @next_id

    next_name: () =>
      @next().name() if @next()?

    to_hash: () =>
      {number: @number(), next: @next_id}

    remove_next: () =>
      @next().remove(false)

    select_step: () =>
      return if not @next()?
      @next().set_as_current()
