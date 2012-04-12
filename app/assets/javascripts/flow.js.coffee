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
      ko.applyBindings

    draw_newline: () =>
      @container.append('<p> </p>')

    draw_empty: (klass="") =>
      @container.append("<div class=\"#{klass}\"><span></span></div>")

    draw_step: (step, klass="") =>
      # TODO: Check if render template is more efficient, or apply binding directly to the step to avoid the get_step call
      # ko.renderTemplate(step.item_template_id(), step, {}, step_node[0]) ?
      step_node = $("<div class=\"#{klass}\" data-bind=\"template: { name: '#{step.item_template_id()}', data: get_step(#{step.id}) }\"> </div>").appendTo(@container)
      ko.applyBindings(workflow, step_node[0])


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

    get_parent: (step) =>
      (parent for parent in @steps() when step.id in parent.next_ids())[0]

    add_step: (command) =>
      @steps.push command

    create_step: (command_type, parent) =>
      new_step = Step.from_hash(type: command_type, id: @generate_id(), root: not parent?)
      @steps.push new_step
      new_step

    remove_step: (step) =>
      @steps.remove(step)
      @initialize_current_step() if @current_step() == step

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

    # private

    generate_id: () =>
      id = new Date().getTime()
      while id in (step.id for step in @steps())
        id += 1
      return id

  # ---------------------------------------------------------------------------

  class CommandSelector
    constructor: ->
      handlers = (new ClassBindingHandler(klass) for klass in [Menu])
      @commands = ko.observableArray(handlers)

    # command_named: (name) =>
    #   (command for command in @commands() when command.name() is name)[0]

    display_template_id: () ->
      'command_selector_template'

    # add_menu_to_steps: () ->
    #   workflow.create_step('menu')

  # ---------------------------------------------------------------------------

  class ClassBindingHandler
    constructor: (cmd)->
      @cmd = cmd
    add_to_steps: =>
      workflow.create_step(@cmd.name, null)
    name: =>
      @cmd.name

  # ---------------------------------------------------------------------------

  class Step
    constructor: (attrs) ->
      @root = false
      @id = attrs['id']
      @root = attrs['root']

    @from_hash: (hash) ->
      item = null
      switch hash['type'].toLowerCase()
        when 'menu'
          item = Menu.from_hash(hash)
        else
          throw "Command type not recognised #{hash['type']}"

      return item

    parent: () =>
      workflow.get_parent(@)

    is_current_step: () =>
      workflow.current_step == @

    remove_with_confirm: () =>
      name = @name?() || "this step"
      if confirm("Are you sure you want to remove #{name} and all steps after it?")
        @remove()

    remove: (notify=true) =>
      @parent().child_removed @ if notify
      workflow.remove_step @

    set_as_current: () =>
      workflow.set_as_current @

    children: () =>
      (step for step in workflow.steps() when step.id in @next_ids())

    item_template_id: () =>
      'workflow_step_template'

    child_removed: (child) =>
      null

  # ---------------------------------------------------------------------------

  class Menu extends Step
    constructor: (attrs) ->
      super(attrs)

      @name = ko.observable attrs['name'] || 'Menu'
      @options = ko.observableArray([])

      @new_option_command = ko.observable null
      @current_editing_message = ko.observable null

      @messages =
        end_call: Message.from_hash(attrs.end_call_message).with_title('End call').with_parent(@)
        invalid:  Message.from_hash(attrs.invalid_message).with_title('Invalid').with_parent(@)
        explanation: Message.from_hash(attrs.explanation_message).with_title('Explanation').with_parent(@)
        options:  Message.from_hash(attrs.options_message).with_title('Options').with_parent(@)

      @is_editing_message = ko.computed () =>
        @current_editing_message() != null

      @available_numbers = ko.computed () =>
        used_numbers = (opt.number() for opt in @options())
        (number for number in [1,2,3,4,5,6,7,8,9,0] when number not in used_numbers)

    display_template_id: () =>
      'menu_step_template'

    button_class: () =>
      'ldial'

    next_ids: () =>
      (option.next_id for option in @options())

    commands: () =>
      (command.name() for command in workflow.commands())

    @add_to_steps: () ->
      workflow.add_step(new Menu)

    @from_hash: (hash) ->
      menu = new Menu(hash)
      menu.options(new MenuOption(opt.number, opt.next, menu) for opt in (hash.options || []))
      return menu

    to_hash: () =>
      id: @id
      name: @name()
      type: 'menu'
      root: @root
      options: (option.to_hash() for option in @options())
      end_call_message: @messages['end_call'].to_hash()
      invalid_message: @messages['invalid'].to_hash()
      explanation_message: @messages['explanation'].to_hash()
      options_message: @messages['options'].to_hash()

    add_option: () =>
      new_step = workflow.create_step(@new_option_command(), @)
      @options.push(new MenuOption(@available_numbers()[0], new_step.id, @))

    remove_option: (option) =>
      if confirm("Are you sure you want to remove option #{option.next().name()} and all steps after it?")
        @options.remove option
        option.remove_next()

    child_removed: (child) =>
      for option in @options()
        if option.next_id == child.id
          @options.remove option
          break

    remove: (notify=true) =>
      for option in @options()
        option.remove_next()
      super(notify)

    message: (msg) =>
      @messages[msg]

    show_message: (msg) =>
      msg = @messages[msg]
      @current_editing_message(msg)

    show_end_call_message: () =>
      @show_message('end_call')

    show_invalid_message: () =>
      @show_message('invalid')

    show_options_message: () =>
      @show_message('options')

    show_explanation_message: () =>
      @show_message('explanation')


  # ---------------------------------------------------------------------------

  class Message
    constructor: (hash={}) ->
      @name = ko.observable hash.name
      @title = ko.observable ""
      @type = ''
      @parent = null

    to_hash: () =>
      name: @name()
      type: @type

    @from_hash: (hash) ->
      #HACK: Handle null case with a base class message
      return new RecordedMessage if not hash? || not hash.type?
      switch hash.type.toLowerCase()
        when 'record'
          new RecordedMessage(hash)
        else
          throw "Message type not recognised #{hash['type']}"

    with_title: (new_title) =>
      @title(new_title)
      return @

    with_parent: (new_parent) =>
      @parent = new_parent
      return @

    back: () =>
      @parent.current_editing_message(null)


  # ---------------------------------------------------------------------------

  class RecordedMessage extends Message
    constructor: (hash={}) ->
      super(hash)
      @file = ko.observable hash.file
      @recording = ko.observable false
      @playing = ko.observable false
      @duration = ko.observable (new Date).clearTime().toString('mm:ss')
      @type = 'record'
      @recording_start = null
      @update_duration_interval = null

    record: () =>
      return if @playing() or @recording()
      @recording true
      @playing false
      @duration (new Date).clearTime().toString('mm:ss')
      Wami.setup
        id: 'wami'
        swfUrl: '/Wami.swf'
        onReady: =>
          Wami.startRecording(save_recording_application_path);
          @recording_start = Math.round(+new Date()/1000)
          @update_duration_interval = window.setInterval((() =>
            @duration((new Date).clearTime().addSeconds(Math.round(+new Date()/1000) - @recording_start).toString('mm:ss'))), 100)
      @alert_flash_required('recording')

    stop: () =>
      if Wami.stopRecording # check if Wami is loaded
        Wami.stopRecording() if @recording()
        Wami.stopPlaying() if @playing()
      @recording(false)
      @playing(false)
      window.clearInterval(@update_duration_interval)

    play: () =>
      return if @playing() or @recording()
      @recording(false)
      @playing(true)
      Wami.setup
        id: 'wami'
        swfUrl: '/Wami.swf'
        onReady: =>
          window.currentMessage = @
          window.playFinished = () ->
            window.setTimeout((() ->
              window.currentMessage.playing(false)), 100)
          Wami.startPlaying(play_recording_application_path, null, 'window.playFinished') # TODO: Use a play path
      @alert_flash_required('playing')

    back: () =>
      @stop()
      super

    to_hash: () =>
      if @file() or @name()?
        $.extend(super,
          file: @file()
          duration: @duration()
        )
      else
        {}

    # private

    alert_flash_required: (action) =>
      if $('.flash-required').length
        $('.flash-required').html('')
        alert "Adobe Flash Player version 10.0.0 or higher is required for #{action} a message.\nDownload it from https://get.adobe.com/flashplayer/ and reload this page."

  # ---------------------------------------------------------------------------

  class MenuOption
    constructor: (num, next_id, menu) ->
      @number = ko.observable num
      @next_id = next_id
      @menu = menu
      @available_numbers = ko.computed () =>
        @menu.available_numbers().concat([@number()]).sort()

    next: () =>
      workflow.get_step @next_id

    next_name: () =>
      @next().name() if @next()?

    to_hash: () =>
      {number: @number(), next: @next_id}

    remove_next: () =>
      @next().remove(false)

    select_step: () =>
      return if not @next()?
      @next().set_as_current()

  # ---------------------------------------------------------------------------

  workflow = new Workflow(new CommandSelector)
  ko.applyBindings(workflow)
  window.workflow = workflow
