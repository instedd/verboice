onWorkflow ->
  class window.ClassBindingHandler
    constructor: (cmd, container)->
      @cmd = cmd
      @container = container

    selected: =>
      @container.requestor.command_selected(@cmd.type)

    name: =>
      (new @cmd({})).name

    button_class: =>
      (new @cmd({})).button_class
