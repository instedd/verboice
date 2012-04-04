# var commands = {"answer":[],"callback":[{"name":"url","type":"string","optional":true},{"name":"method","type":"string","optional":true,"default":"post"}],"capture":[{"name":"min","type":"integer","default":1,"ui_length":1},{"name":"max","type":"integer","default":1,"ui_length":1},{"name":"finish_on_key","type":"string","default":"#","ui_length":1},{"name":"timeout","type":"integer","default":5,"ui_length":1},{"name":"play","type":"string","ui_length":40},{"name":"say","type":"string","ui_length":40}],"dial":[{"name":"number","type":"string","ui_length":80},{"name":"channel","type":"string","ui_length":20}],"hangup":[],"pause":[{"name":"length","type":"integer","default":1,"ui_length":3}],
# "play_url":[{"name":"url","type":"string","ui_length":80}],"record":[],"say":[{"name":"text","type":"string","ui_length":80}]};
# var flow = ["hangup",{"play_url":""},{"capture":{"min":"1","max":"1","finish_on_key":"#","timeout":"5","play":"","say":""}},{"play_url":""}];

# TODO: Change the array representation to a linked list to support the 'if' command

# TODO: Split this into multiple files. One file per class

jQuery ->
  if not $('#workflow').length > 0
    return

  # ---------------------------------------------------------------------------

  class WorkflowDrawer
    constructor: (container) ->
      @container = $(container)

    draw_workflow: (steps) =>
      @matrix_yx = []
      @container.empty()
      y = 0
      roots = (step for step in steps when step.root)
      for root in roots
        [_x,y] = @recursive_draw_workflow(root, 0, y)
      @draw_matrix()

    recursive_draw_workflow: (step, x, y, parent_x, parent_y, klass='ha') =>
      @set_step(step, x, y, klass)
      [next_x, next_y] = [x+1, y]
      klass = 'ha'
      for child in step.children()
        @fill_va_ext(next_x, y, next_y) if klass == 'va'
        [_x, next_y] = @recursive_draw_workflow(child, next_x, next_y, x, y, klass)
        klass = 'va'
      next_y = y+1 if next_y < y+1
      return [x, next_y]

    fill_va_ext: (x, from_y, to_y) =>
      for y_i in [from_y..to_y]
        @matrix_yx[y_i] ?= []
        if not @matrix_yx[y_i][x]? or (@matrix_yx[y_i][x][0] == false and @matrix_yx[y_i][x][1] == '')
          @matrix_yx[y_i][x] = [false, 'va-ext']

    set_step: (step, x, y, klass) =>
      @matrix_yx[y] ?= []
      for x_i in [0..x-1]
        if not @matrix_yx[y][x_i]?
          @matrix_yx[y][x_i] = [false, '']
      @matrix_yx[y][x] = [step, klass]

    draw_matrix: () =>
      for row in @matrix_yx
        @draw_newline()
        for [elem, klass] in row
          if elem == false
            @draw_empty(klass)
          else
            @draw_step(elem, klass)

    draw_newline: () =>
      @container.append('<p> </p>')

    draw_empty: (klass="") =>
      @container.append("<div class=\"#{klass}\"><span></span></div>")

    draw_step: (step, klass="") =>
      @container.append("<div class=\"#{klass}\" data-bind=\"template: { name: '#{step.item_template_id()}', data: get_step(#{step.id}) }\"> </div>")

  # ---------------------------------------------------------------------------

  ko.bindingHandlers.workflow_steps =
    init: (element, valueAccessor, allBindingsAccessor, viewModel) ->
      container = $("<div class='workflow-container'></div>").appendTo(element)
      viewModel.workflow_drawer = new WorkflowDrawer(container)
    update: (element, valueAccessor, allBindingsAccessor, viewModel) ->
      steps = ko.utils.unwrapObservable(valueAccessor())
      viewModel.workflow_drawer.draw_workflow(steps)

  # ---------------------------------------------------------------------------

  ko.bindingHandlers.class =
    update: (element, valueAccessor) ->
      if (element['__ko__previousClassValue__'])
          $(element).removeClass(element['__ko__previousClassValue__'])
      value = ko.utils.unwrapObservable(valueAccessor())
      $(element).addClass(value)
      element['__ko__previousClassValue__'] = value

  # ---------------------------------------------------------------------------

  class Workflow
    constructor: (command_selector) ->
      @steps = ko.observableArray(Step.from_hash(hash) for hash in application_flow)
      @command_selector = ko.observable(command_selector)
      @current_step = ko.observable @command_selector()

    get_step: (id) =>
      (step for step in @steps() when step.id == id)[0]

    add_step: (command) =>
      @steps.push command

    remove_step: (step) =>
      @steps.remove(step)
      @initialize_current_step()

    set_as_current: (step) =>
      @current_step(step)

    initialize_current_step: () =>
      @set_as_current(@command_selector())

    display_template_for: (current_flow_step) =>
      @current_step().display_template_id()

    serialize_workflow: () =>
      serialized = JSON.stringify(step.to_hash() for step in @steps())
      $('#flow').val(serialized)
      return true # let the submit handler do its work

    commands: () =>
      @command_selector().commands()

  # ---------------------------------------------------------------------------

  class CommandSelector
    constructor: ->
      handlers = (new ClassBindingHandler(klass) for klass in [Menu])
      @commands = ko.observableArray(handlers)

    # command_named: (name) =>
    #   (command for command in @commands() when command.name() is name)[0]

    display_template_id: () ->
      'command_selector_template'

    add_menu_to_steps: () ->
      workflow.add_step(new Menu)

  # ---------------------------------------------------------------------------

  class ClassBindingHandler
    constructor: (cmd)->
      @cmd = cmd
    add_to_steps: =>
      @cmd.add_to_steps()

  # ---------------------------------------------------------------------------

  class Step
    constructor: () ->
      @root = false

    @from_hash: (hash) ->
      item = null
      switch hash['type']
        when 'menu'
          item = Menu.from_hash(hash)
        else
          throw "Command type not recognised #{hash['type']}"

      item.root = hash['root']
      item.id = hash['id']
      return item

    is_current_step: () =>
      workflow.current_step == @

    remove: () =>
      workflow.remove_step @

    set_as_current: () =>
      workflow.set_as_current @

    children: () =>
      (step for step in workflow.steps() when step.id in @next_ids())

    item_template_id: () =>
      'workflow_step_template'

  # ---------------------------------------------------------------------------

  class Menu extends Step
    constructor: (name, options) ->
      @name = ko.observable name || 'Menu'
      @options = ko.observableArray(options)

    display_template_id: () =>
      'menu_step_template'

    button_class: () =>
      'ldial'

    start_recording: () =>
      Wami.setup
        id: 'wami'
        swfUrl: '/Wami.swf'
        onReady: ->
          Wami.startRecording(save_recording_application_path);
      if $('.flash-required').length
        $('.flash-required').html('')
        alert "Adobe Flash Player version 10.0.0 or higher is required for recording a message.\nDownload it from https://get.adobe.com/flashplayer/ and reload this page."

    stop_recording: () =>
      Wami.stopRecording() if Wami.stopRecording

    next_ids: () =>
      (option.next() for option in @options())

    @add_to_steps: () ->
      workflow.add_step(new Menu)

    @from_hash: (hash) ->
      options = (new MenuOption(opt.number, opt.description, opt.next) for opt in (hash.options || []))
      menu = new Menu(hash['name'], options)
      return menu

    to_hash: () =>
      {name: @name(), type: 'menu', root: @root, id: @id, options: (option.to_hash() for option in @options())}

    remove_option: (option) =>
      @options.remove option

    add_option: () =>
      @options.add new MenuOption()

    commands: () =>
      workflow.commands()

  # ---------------------------------------------------------------------------

  class MenuOption
    constructor: (number, description, next) ->
      @number = ko.observable number
      @description = ko.observable description
      @next = ko.observable next

    to_hash: () =>
      {number: @number(), description: @description(), next: @next()}

  # ---------------------------------------------------------------------------

  workflow = new Workflow(new CommandSelector)
  ko.applyBindings(workflow)


  # Wami.startPlaying(anyWavURL);
  # Wami.stopPlaying();