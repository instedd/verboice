#= require workflow/steps/input_setting

onWorkflow ->
  class window.MbuilderStepSetting extends window.InputSetting
    constructor: (parent, attrs) ->
      super(attrs)

      @name = attrs.name
      @parent = parent

    to_hash: () =>
      $.extend super(), {
        name: @name
      }
