# var commands = {"answer":[],"callback":[{"name":"url","type":"string","optional":true},{"name":"method","type":"string","optional":true,"default":"post"}],"capture":[{"name":"min","type":"integer","default":1,"ui_length":1},{"name":"max","type":"integer","default":1,"ui_length":1},{"name":"finish_on_key","type":"string","default":"#","ui_length":1},{"name":"timeout","type":"integer","default":5,"ui_length":1},{"name":"play","type":"string","ui_length":40},{"name":"say","type":"string","ui_length":40}],"dial":[{"name":"number","type":"string","ui_length":80},{"name":"channel","type":"string","ui_length":20}],"hangup":[],"pause":[{"name":"length","type":"integer","default":1,"ui_length":3}],
# "play_url":[{"name":"url","type":"string","ui_length":80}],"record":[],"say":[{"name":"text","type":"string","ui_length":80}]};
# var flow = ["hangup",{"play_url":""},{"capture":{"min":"1","max":"1","finish_on_key":"#","timeout":"5","play":"","say":""}},{"play_url":""}];

jQuery ->
  if not $('ul#workflow').length > 0
    return

  class FlowViewModel
    constructor: ->
      @steps = ko.observableArray(StepViewModel.from_data(data) for data in flow)

    add_step: (command) =>
      @steps.push StepViewModel.from_command(command)

  class StepViewModel
    constructor: (command, arguments) ->
      debugger;
      @command = ko.observable command
      @name = ko.computed(=> @command.name())
      @arguments = ko.observableArray(create_arguments(arguments))
      
    create_arguments: (single_arg_value) =>
      # There is only one argument due to play_url assumption, and it is a string value
      # ToDo: match args depending on definition name
      for definition in @command.definitions
        new ArgumentViewModel(definition, single_arg_value)
        
    @from_command: (command) =>
      new this(command, null)
      
    @from_data: (data) =>
      # Assume data is in the form of {name: single_param}
      # ToDo: Support 'name' and {name: {param1: 'val1', param2: 'val2'}}
      [name, args] = ([name, args] for name, args of data)[0]
      command = commands_model.command_named(name)
      new this(command, args)
            
  class ArgumentViewModel
    constructor: (definition, value) ->
      @definition = ko.observable definition
      @value = ko.observable value
      
  class ArgumentDefinitionViewModel
    constructor: (data) ->
      @name = ko.observable data.name
      @optional = ko.observable data.optional
      @type = ko.observable data.type
      @ui_length = data.ui_length
  
  class CommandsViewModel
    constructor: () ->
      @commands = ko.observableArray(new CommandViewModel(name, template) for name, template of commands)

    command_named: (name) =>
      command for command in @commands() when command.name is name
      
  class CommandViewModel
    constructor: (name, data) ->
      @name = ko.observable name
      @definitions = ko.observableArray(new ArgumentDefinitionViewModel(definition) for definition in data)

    add_to_steps: () =>
      flow_model.add_step(@)
  
  commands_model = new CommandsViewModel
  flow_model = new FlowViewModel
  
  ko.applyBindings(flow_model, document.getElementById('workflow'))
  ko.applyBindings(commands_model, document.getElementById('command-list'))

  # $('#command-list li').live 'click', ->
  #   flow_model.add_step $(@).text().trim()

  # $('#workflow li a').live 'click', ->
  #   $(@).parent().remove()