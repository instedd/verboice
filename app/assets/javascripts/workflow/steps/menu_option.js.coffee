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
      @next()?.name()

    to_hash: () =>
      {number: @number(), next: @next_id}

    remove_next: () =>
      next = @next()
      if next
        @next_id = null
        next.remove_recursive()

    select_step: () =>
      return if not @next()?
      @next().set_as_current()

    child_removed: () =>
      console.log ("Child removed")
      @menu.remove_option(@)