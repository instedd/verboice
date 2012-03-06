class FlowViewModel
  constructor: (data) ->
    @commands = ko.observableArray( for name, some_data of data
      new CommandViewModel(name, some_data) )

class StepViewModel
  constructor: (data) ->
    @commands = ko.observableArray( for name, some_data of data
      new CommandViewModel(name, some_data) )


class CommandViewModel
  constructor: (name, data) ->
    @name = ko.observable name
    @template = ko.observable data

jQuery ->
  return unless $('#workflow').length

  # class FlowViewModel
  ko.applyBindings(new FlowViewModel(flow))
  
  
