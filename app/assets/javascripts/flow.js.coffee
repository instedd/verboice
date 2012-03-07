class FlowViewModel

  constructor: ->
    @commands = ko.observableArray( for name, template of commands
      new CommandViewModel(name, template))
    @steps = ko.observableArray( flow )

  command_named: (name) ->
    (command for command in @commands when command.name == name)[0]

  add_step: (step) ->
    @steps.push new StepViewModel (@command_named(step))

class StepViewModel
  constructor: (command) ->
    @command = ko.observable command
  name: ->
    @command.name
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