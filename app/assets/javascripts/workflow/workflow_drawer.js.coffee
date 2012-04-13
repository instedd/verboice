onWorkflow ->
  class window.WorkflowDrawer
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
