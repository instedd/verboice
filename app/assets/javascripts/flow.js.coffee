#= require_tree ./workflow

onWorkflow ->

  ko.bindingHandlers.workflow_steps =
    init: (element, valueAccessor, allBindingsAccessor, viewModel) ->
      container = $("<div class='workflow-container'></div>").appendTo(element)
      viewModel.workflow_drawer = new WorkflowDrawer(container)
    update: (element, valueAccessor, allBindingsAccessor, viewModel) ->
      steps = ko.utils.unwrapObservable(valueAccessor())
      viewModel.workflow_drawer.draw_workflow(steps)

  ko.bindingHandlers.instedd_init =
    init: (element, valueAccessor, allBindingsAccessor, viewModel) ->
      $.instedd.init_components($(element))

  ko.bindingHandlers.class =
    update: (element, valueAccessor) ->
      if (element['__ko__previousClassValue__'])
          $(element).removeClass(element['__ko__previousClassValue__'])
      value = ko.utils.unwrapObservable(valueAccessor())
      $(element).addClass(value)
      element['__ko__previousClassValue__'] = value

  window.step_types = [Play, Menu, Capture, Transfer, Goto, Branch, HangUp, Record]
  for step_type in window.step_types
    window[step_type.type] = step_type

  window.workflow = new Workflow(new CommandSelector)
  window.workflow.after_initialize()
  ko.applyBindings(workflow)

  window.workflow.serialize_workflow()
  $(window).bind 'beforeunload', () ->
    if window.workflow.has_changed()
      "There are unsaved changes in the workflow."

  $(window).resize () ->
    container_width = $('#container').width()
    $('.workflow-content-container').width(container_width - 364)
  .resize()

