#= require workflow/steps/input_setting

onWorkflow ->
  class window.ExternalStepSetting extends window.InputSetting
    constructor: (parent, attrs) ->
      super(attrs)

      @name = attrs.name
      @display_name = attrs.display_name
      @parent = parent

    to_hash: () =>
      $.extend super(), {
        name: @name
        display_name: @display_name
      }

    description: () =>
      desc = super()
      if desc? then "(#{desc})" else null

    on_begin_edition: () =>
      @content_kind_tmp = @content_kind()
      @value_tmp = @value()
      @variable_tmp = @variable()
      @step_id_tmp = @step_id()

    save: () =>
      @exit()

    cancel: () =>
      @content_kind(@content_kind_tmp)
      @value(@value_tmp)
      @variable(@variable_tmp)
      @step_id(@step_id_tmp)
      @exit()

    exit: () =>
      @parent.current_editing_setting(null)
