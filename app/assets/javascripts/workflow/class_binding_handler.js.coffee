onWorkflow ->
  class window.ClassBindingHandler
    constructor: (cmd)->
      @cmd = cmd
    add_to_steps: =>
      parent = workflow.current_step()?.parent
      new_step = workflow.create_step(@cmd.type, parent)
      workflow.set_as_current(new_step)
    name: =>
      (new @cmd({})).name
    button_class: =>
      (new @cmd({})).button_class
