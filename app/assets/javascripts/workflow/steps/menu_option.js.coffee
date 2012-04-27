onWorkflow ->
  class window.MenuOption
    constructor: (num, next_id, menu) ->
      @skip_step = null
      @number = ko.observable((num || "").toString())
      @next_id = next_id
      @menu = menu
      @available_numbers = ko.computed () =>
        @menu.available_numbers().concat([@number()]).sort()

    next: () =>
      workflow.get_step @next_id

    next_name: () =>
      if @next()? then @next().name() else "Skip to #{if @menu.next_id > 0 then @menu.next().name() else 'next step'}"

    skip: () =>
      @skip_step ?= new Skip

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
      @menu.remove_option(@)