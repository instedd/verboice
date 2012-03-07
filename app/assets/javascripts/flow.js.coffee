class FlowViewModel

  constructor: ->
    @commands = ko.observableArray( for name, template of commands
      new CommandViewModel(name, template) )
    @steps = ko.observableArray( flow )

  command_named: (name) ->
    command = for command in @commands() when command.name == name
      command

  add_step: (step) ->
    alert @commands
    alert @commands()
    alert step
    foo = for index, com in @commands() when command.name == step
      com
    alert foo[0].name

    alert @command_named(step)
    @steps.push new StepViewModel (@command_named(step))

class StepViewModel
  constructor: (command) ->
    @command = ko.observable command
    @name = ko.observable @command.name
  arguments: ->
    @command.template()

class CommandViewModel
  constructor: (name, data) ->
    @name = ko.observable name
    @template = ko.observable data

jQuery ->
  flow_model = new FlowViewModel
  ko.applyBindings(flow_model)

  $('#command-list li').live 'click', ->
    flow_model.add_step $(@).text().trim()

  $('#workflow li a').live 'click', ->
    $(@).parent().remove()