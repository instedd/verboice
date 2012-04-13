#= require_directory ./workflow

onWorkflow ->

  ko.bindingHandlers.workflow_steps =
    init: (element, valueAccessor, allBindingsAccessor, viewModel) ->
      container = $("<div class='workflow-container'></div>").appendTo(element)
      viewModel.workflow_drawer = new WorkflowDrawer(container)
    update: (element, valueAccessor, allBindingsAccessor, viewModel) ->
      steps = ko.utils.unwrapObservable(valueAccessor())
      viewModel.workflow_drawer.draw_workflow(steps)

  ko.bindingHandlers.class =
    update: (element, valueAccessor) ->
      if (element['__ko__previousClassValue__'])
          $(element).removeClass(element['__ko__previousClassValue__'])
      value = ko.utils.unwrapObservable(valueAccessor())
      $(element).addClass(value)
      element['__ko__previousClassValue__'] = value

  class window.ClassBindingHandler
    constructor: (cmd)->
      @cmd = cmd
    add_to_steps: =>
      workflow.create_step(@cmd.name, null)
    name: =>
      @cmd.name

  window.workflow = new Workflow(new CommandSelector)
  ko.applyBindings(workflow)

