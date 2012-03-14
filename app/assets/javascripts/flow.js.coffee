# var commands = {"answer":[],"callback":[{"name":"url","type":"string","optional":true},{"name":"method","type":"string","optional":true,"default":"post"}],"capture":[{"name":"min","type":"integer","default":1,"ui_length":1},{"name":"max","type":"integer","default":1,"ui_length":1},{"name":"finish_on_key","type":"string","default":"#","ui_length":1},{"name":"timeout","type":"integer","default":5,"ui_length":1},{"name":"play","type":"string","ui_length":40},{"name":"say","type":"string","ui_length":40}],"dial":[{"name":"number","type":"string","ui_length":80},{"name":"channel","type":"string","ui_length":20}],"hangup":[],"pause":[{"name":"length","type":"integer","default":1,"ui_length":3}],
# "play_url":[{"name":"url","type":"string","ui_length":80}],"record":[],"say":[{"name":"text","type":"string","ui_length":80}]};
# var flow = ["hangup",{"play_url":""},{"capture":{"min":"1","max":"1","finish_on_key":"#","timeout":"5","play":"","say":""}},{"play_url":""}];

# TODO: Change the array representation to a linked list to support the 'if' command

jQuery ->
  if not $('#workflow').length > 0
    return

  class Workflow
    constructor: (commands_model)->
      @steps = ko.observableArray(Step.from_data(data, commands_model) for data in application_flow)
      @commands = ko.observable(commands_model)
      @current_step = ko.observable(commands_model)

    add_step: (command) =>
      @steps.push Step.from_command(command)

    remove_step: (step) =>
      @steps.remove(step)
      @reset_current()

    set_as_current: (step) =>
      @current_step(step)

    reset_current: () =>
      @set_as_current(@commands())

    display_template_for: (current_flow) =>
      @current_step().display_template_id()

    # Persist change on the server
    submitChange: =>

      $.ajax {
        type: 'POST',
        url: update_workflow_application_path,
        data: {
          _method: 'PUT',
          flow: @flow_array()
        },
        success: (data) ->
          window.location = application_path;
        dataType: 'json'
      }

    flow_array: () =>
      output = new Array
      for step in @steps()
        args = new Object
        flow_step= new Object

        for arg in step.arguments()
          args[arg.name()] = arg.value()

        flow_step[step.name()] = args
        output.push(flow_step)
      output

  class Step
    constructor: (command, arguments) ->
      @command = ko.observable command
      @name = ko.computed(=> @command().name())
      @arguments = ko.observableArray(@create_arguments(arguments))

    create_arguments: (initial_args) =>
      args = for definition in (@command().definitions())
        initial_value = if initial_args
          initial_args[definition.name()]
        else
          definition.default_value
        new Argument(definition, initial_value)
      args

    remove: () =>
      workflow.remove_step @

    set_as_current: () =>
      workflow.set_as_current @

    display_template_for: (argument) =>
      argument.data_type()

    @from_command: (command) =>
      new this(command, null)

    @from_data: (data, commands_model) =>
      [name, args] = ([name, args] for name, args of data)[0]
      command = commands_model.command_named(name)
      new this(command, args)

    display_template_id: () =>
      'step_template'

  class Argument
    constructor: (definition, value) ->
      @definition = ko.observable definition
      @value = ko.observable value

    name: =>
      @definition().name()
    data_type: =>
      @definition().data_type()

    display_template_for: () =>
      alert 'wtf?'

  class ArgumentDefinition
    constructor: (data) ->
      @name = ko.observable data.name
      @optional = ko.observable data.optional
      @data_type = ko.observable data.type
      @ui_length = data.ui_length
      @default_value = data.default

    display_template_for: () =>
      alert 'wtf 3?'

  class Commands
    constructor: () ->
      @commands = ko.observableArray(new Command(name, template) for name, template of commands)

    command_named: (name) =>
      (command for command in @commands() when command.name() is name)[0]

    display_template_id: () =>
      'command_selector_template'

  class Command
    constructor: (name, data) ->
      @name = ko.observable name
      @definitions = ko.observableArray(new ArgumentDefinition(definition) for definition in data)

    add_to_steps: () =>
      workflow.add_step(@)

    display_template_for: () =>
      alert 'wtf 2?'
    button_class: () =>
      'lcallback'

  workflow = new Workflow(new Commands)

  ko.applyBindings(workflow)