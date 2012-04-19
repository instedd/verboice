onWorkflow ->
  class window.ClassBindingHandler
    constructor: (cmd)->
      @cmd = cmd
    add_to_steps: =>
      parent = workflow.current_step()?.parent
      new_step = workflow.create_step(@cmd.name, parent)
      workflow.set_as_current(new_step)
    name: =>
      @cmd.name
    button_class: =>
      (new @cmd({})).button_class
