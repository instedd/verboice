# var commands = {"answer":[],"callback":[{"name":"url","type":"string","optional":true},{"name":"method","type":"string","optional":true,"default":"post"}],"capture":[{"name":"min","type":"integer","default":1,"ui_length":1},{"name":"max","type":"integer","default":1,"ui_length":1},{"name":"finish_on_key","type":"string","default":"#","ui_length":1},{"name":"timeout","type":"integer","default":5,"ui_length":1},{"name":"play","type":"string","ui_length":40},{"name":"say","type":"string","ui_length":40}],"dial":[{"name":"number","type":"string","ui_length":80},{"name":"channel","type":"string","ui_length":20}],"hangup":[],"pause":[{"name":"length","type":"integer","default":1,"ui_length":3}],
# "play_url":[{"name":"url","type":"string","ui_length":80}],"record":[],"say":[{"name":"text","type":"string","ui_length":80}]};
# var flow = ["hangup",{"play_url":""},{"capture":{"min":"1","max":"1","finish_on_key":"#","timeout":"5","play":"","say":""}},{"play_url":""}];

# TODO: Change the array representation to a linked list to support the 'if' command

jQuery ->
  if not $('#workflow').length > 0
    return

  class Workflow
    constructor: (command_selector)->
      # @steps = ko.observableArray(Step.from_data(data, commands_model) for data in application_flow)
      @steps = ko.observableArray([])
      @command_selector = ko.observable(command_selector)
      @current_step = ko.observable @command_selector()

    add_step: (command) =>
      @steps.push command
      # @steps.push Step.from_command(command)

    remove_step: (step) =>
      @steps.remove(step)
      @initialize_current_step()

    set_as_current: (step) =>
      @current_step(step)

    initialize_current_step: () =>
      @set_as_current(@command_selector())

    display_template_for: (current_flow_step) =>
      @current_step().display_template_id()

    # Persist change on the server
    submitChange: () =>
    #   $.ajax {
    #     type: 'POST',
    #     url: update_workflow_application_path,
    #     data: {
    #       _method: 'PUT',
    #       flow: @flow_array()
    #     },
    #     success: (data) ->
    #       window.location = application_path;
    #     dataType: 'json'
    #   }

    # flow_array: () =>
    #   output = new Array
    #   for step in @steps()
    #     args = new Object
    #     flow_step= new Object

    #     for arg in step.arguments()
    #       args[arg.name()] = arg.value()

    #     flow_step[step.name()] = args
    #     output.push(flow_step)
    #   output

  class CommandSelector
    constructor: ->
      @commands = ko.observableArray([new WhatShouldBeAClass(Menu)])

    # command_named: (name) =>
    #   (command for command in @commands() when command.name() is name)[0]

    display_template_id: () ->
      'command_selector_template'

    add_menu_to_steps: () ->
      workflow.add_step(new Menu)

  class WhatShouldBeAClass
    constructor: (cmd)->
      @cmd = cmd
    add_to_steps: =>
      @cmd.add_to_steps()

  class Menu
    constructor: () ->
      @name = ko.observable 'My Menu'

    is_current_step: () =>
      workflow.current_step == @

    remove: () =>
      workflow.remove_step @

    set_as_current: () =>
      workflow.set_as_current @

    display_template_id: () =>
      'menu_step_template'

    @add_to_steps: () ->
      workflow.add_step(new Menu)

    button_class: () ->
      'ldial'

  ko.bindingHandlers['class'] = {
    'update': (element, valueAccessor) ->
      if (element['__ko__previousClassValue__'])
          $(element).removeClass(element['__ko__previousClassValue__'])
      value = ko.utils.unwrapObservable(valueAccessor())
      $(element).addClass(value)
      element['__ko__previousClassValue__'] = value
  }
  workflow = new Workflow(new CommandSelector)
  ko.applyBindings(workflow)