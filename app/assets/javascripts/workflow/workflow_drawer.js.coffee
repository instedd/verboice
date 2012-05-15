onWorkflow ->
  class window.WorkflowDrawer
    constructor: (container) ->
      @container = $(container)

    # Arrows classes:
    #  ha ->        Horizontal arrow, short
    #  ha-ext ->    Horizontal arrow, extended
    #  ha-ext-nodot -> Horizontal arrow, extended, without initial dot
    #  va-bl-t ->   Vertical arrow, from Bottom Left to Top
    #  va-bl-tr ->  Vertical arrow, from Bottom Left to Top Right
    draw_workflow: (steps) =>
      #console.log 'Redrawing workflow'
      @matrix_ij = []
      @container.empty()

      i = 0
      roots = (step for step in steps when step.root)

      for root in roots
        [i,_j] = @recursive_draw_workflow
          step: root
          i: i
          j: 0
          klass: 'root'

      @draw_matrix()

    # Recursively draws the workflow by placing 'step' in position 'i', 'j' with class 'klass'
    #  Merge optional parameter indicates if a va-merge class should be added, which corresponds to either a branch or merge back from/to normal flow
    #  Has closure indicates if there is later any step which does a closure
    recursive_draw_workflow: ({step, parent, i, j, klass, merge, ancestor_has_closure}) =>
      klass ?= 'ha'
      merge ?= false
      ancestor_has_closure ?= false
      has_closure = (step.children and step.children().length > 0 and step.next()?)

      @set_step(step, i, j, klass, merge)
      [next_i, next_j] = [i,j+1]

      if step.children?
        klass = 'ha'
        next_merge = false

        for child in step.children()
          unless child.type() == 'skip' and not (has_closure or ancestor_has_closure)
            last_child_i = next_i

            [next_i, child_next_j] = @recursive_draw_workflow
              step: child
              parent: step
              i: next_i
              j: j+1
              klass: klass
              merge: next_merge
              ancestor_has_closure: ancestor_has_closure or has_closure

            next_j = child_next_j if next_j < child_next_j
            next_merge = ((klass == 'ha' or klass == 'root') and next_i == i+1)
            klass = 'va'

        @fill_vertical(j+1, i+1, last_child_i)


      if step.next()?
        parents_first_child_cannot_continue = step.children? and step.children()[0]? and not step.children()[0].can_continue()
        has_closure

        [next_step_i, next_step_j] = [i, next_j]
        [child_next_i, child_next_j] = @recursive_draw_workflow
          step: step.next()
          parent: step
          i: next_step_i
          j: next_step_j
          klass: if parents_first_child_cannot_continue then '' else 'ha'
          ancestor_has_closure: ancestor_has_closure

        next_j = child_next_j if next_j < child_next_j
        next_i = child_next_i if next_i < child_next_i

        if step.children? and step.children().length > 0
          max_child_i = 0
          for child in step.children()
            for leaf in child.leaves()
              if leaf.can_continue() and leaf.position?
                @fill_horizontal(leaf.position[0], leaf.position[1]+1, next_step_j-1)
                next_merge = (leaf.position[0] == i+1)
                next_klass = if next_merge and parents_first_child_cannot_continue then next_merge = false; 'va-bl-tr' else 'va-bl-t'
                @set_step(null, leaf.position[0], next_step_j, next_klass, next_merge)
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
          merge = (i_k == from_i)
          @set_step(null, i_k, j, 'va-ext', merge)

    set_step: (step, i, j, klass, merge=false) =>
      for i_k in [0..i]
        @matrix_ij[i_k] ?= []
      for j_k in [0..j-1]
        if not @matrix_ij[i][j_k]?
          @matrix_ij[i][j_k] = [null, '']

      klass = (if klass == 'va' then 'va-skip' else 'ha-ext') if step?.type() == 'skip'
      klass = "#{klass} va-merge" if merge

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
      @container.append("<div class=\"#{klass}\"><span></span></div>")

    draw_step: (step, klass="") =>
      # TODO: Check if render template is more efficient, or apply binding directly to the step to avoid the get_step call
      # ko.renderTemplate(step.item_template_id(), step, {}, step_node[0]) ?
      step_node = $("<div class=\"#{klass}\" data-bind=\"template: { name: '#{step.item_template_id()}', data: get_step(#{step.id}) }\"> </div>").appendTo(@container)
      ko.applyBindings(workflow, step_node[0])
