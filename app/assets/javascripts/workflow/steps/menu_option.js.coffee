#= require workflow/steps/child_step

onWorkflow ->
  class window.MenuOption extends ChildStep
    constructor: (num, next_id, menu) ->
      super(next_id, menu)

      @number = ko.observable((num || "").toString())
      @available_numbers = ko.computed () =>
        menu.available_numbers().concat([@number()]).sort()

    to_hash: () =>
      $.extend(super,
        number: @number()
      )
