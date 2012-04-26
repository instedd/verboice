onWorkflow ->
  class window.WorkflowDrawer
    constructor: (container) ->
      @container = $(container)

    draw_workflow: (steps) =>
      #console.log 'Redrawing workflow'
      @matrix_ij = []
      @container.empty()
      i = 0
      roots = (step for step in steps when step.root)
      for root in roots
        [i,_j] = @recursive_draw_workflow(root, i, 0)
      @draw_matrix()

    recursive_draw_workflow: (step, i, j, parent_i, parent_j, klass='ha') =>
      @set_step(step, i, j, klass)
      [next_i, next_j] = [i,j+1]

      if step.children?
        child_index = 0
        klass = 'ha'
        for child in step.children()
          last_child_i = next_i
          [next_i, child_next_j] = @recursive_draw_workflow(child, next_i, j+1, i, j, klass)
          next_j = child_next_j if next_j < child_next_j
          klass = if klass == 'ha' and next_i == i+1 then 'va va-merge' else 'va'
        @fill_vertical(j+1, i+1, last_child_i) unless klass == 'ha'

      if step.next?()?
        [next_step_i, next_step_j] = [i, next_j]
        [child_next_i, child_next_j] = @recursive_draw_workflow(step.next(), next_step_i, next_step_j, i, j, 'ha')
        next_j = child_next_j if next_j < child_next_j
        next_i = child_next_i if next_i < child_next_i

        if step.children? and step.children().length > 0
          max_child_i = 0
          for child in step.children()
            for leaf in child.leaves()
              unless leaf.type() == 'goto'
                @fill_horizontal(leaf.position[0], leaf.position[1]+1, next_step_j-1)
                va_merge = if leaf.position[0] == i+1 then 'va-merge' else ''
                @set_step(null, leaf.position[0], next_step_j, "va3 #{va_merge}")
                max_child_i = leaf.position[0] if leaf.position[0] > max_child_i
          @fill_vertical(next_step_j, next_step_i+1, max_child_i-1)

      next_i = i+1 if next_i < i+1
      return [next_i, next_j]

    fill_horizontal: (i, from_j, to_j) =>
      if from_j <= to_j
        for j_k in [from_j..to_j]
          klass = if j_k == from_j then 'ha-ext' else 'ha-ext-nodot'
          @set_step(null, i, j_k, klass)

    fill_vertical: (j, from_i, to_i) =>
      if from_i <= to_i
        for i_k in [from_i..to_i]
          va_merge = if i_k == from_i then 'va-merge' else ''
          @set_step(null, i_k, j, "va-ext #{va_merge}")

    set_step: (step, i, j, klass) =>
      for i_k in [0..i]
        @matrix_ij[i_k] ?= []
      for j_k in [0..j-1]
        if not @matrix_ij[i][j_k]?
          @matrix_ij[i][j_k] = [null, '']

      if step?
        @matrix_ij[i][j] = [step, klass]
        step.position = [i,j]
      else if (not @matrix_ij[i][j]?) or (@matrix_ij[i][j][0] == null and @matrix_ij[i][j][1] == '')
        @matrix_ij[i][j] = [null, klass]

    draw_matrix: () =>
      for row in @matrix_ij
        @draw_newline()
        for pair in row
          if pair?
            [elem, klass] = pair
            if not elem?
              @draw_empty(klass)
            else if elem.type() == 'skip'
              @draw_skip(elem, klass)
            else
              @draw_step(elem, klass)
      ko.applyBindings

    draw_newline: () =>
      @container.append('<p> </p>')

    draw_empty: (klass="") =>
      @container.append("<div class=\"#{klass}\"><span></span></div>")

    draw_skip: (step, klass="") =>
      klass = if klass == 'va' then 'va-skip' else 'ha-ext'
      @container.append("<div class=\"#{klass}\"><span></span></div>")

    draw_step: (step, klass="") =>
      # TODO: Check if render template is more efficient, or apply binding directly to the step to avoid the get_step call
      # ko.renderTemplate(step.item_template_id(), step, {}, step_node[0]) ?
      step_node = $("<div class=\"#{klass}\" data-bind=\"template: { name: '#{step.item_template_id()}', data: get_step(#{step.id}) }\"> </div>").appendTo(@container)
      ko.applyBindings(workflow, step_node[0])
