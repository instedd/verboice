onWorkflow ->
  class window.CommandSelector
    constructor: ->
      handlers = (new ClassBindingHandler(klass) for klass in [Menu])
      @commands = ko.observableArray(handlers)

    # command_named: (name) =>
    #   (command for command in @commands() when command.name() is name)[0]

    display_template_id: () ->
      'command_selector_template'

    # add_menu_to_steps: () ->
    #   workflow.create_step('menu')
