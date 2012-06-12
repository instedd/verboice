onWorkflow ->
  class window.ClassBindingHandler
    constructor: (cmd, container)->
      @cmd = cmd
      @container = container

    selected: =>
      @container.requestor.command_selected(@cmd.type)

    # TODO: It should NOT be necessary to instantiate a command every time we want to access one of these methods
    name: =>
      (new @cmd({})).name

    button_class: =>
      (new @cmd({})).button_class

    background_style: =>
      (new @cmd({})).background_style()
