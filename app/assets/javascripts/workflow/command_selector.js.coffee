onWorkflow ->
  class window.CommandSelector
    constructor: ->
      handlers = (new ClassBindingHandler(window[klass]) for klass in step_types)
      @commands = ko.observableArray(handlers)

    display_template_id: () ->
      'command_selector_template'

    # command_named: (name) =>
      #   (command for command in @commands() when command.name() is name)[0]

    # add_menu_to_steps: () ->
    #   workflow.create_step('menu')
