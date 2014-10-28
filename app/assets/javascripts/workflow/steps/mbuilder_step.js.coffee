#= require workflow/steps/mbuilder_step_setting

onWorkflow ->
  class window.Mbuilder extends Step
    @type = 'mbuilder'

    constructor: (attrs) ->
      super(attrs)

      @loading_actions = ko.observable(true)
      @actions = ko.observableArray()

      @action = ko.observable()
      @settings = ko.observableArray()

      window.setTimeout =>
        @actions.push(
          {
            action : "mbuilder app - send invites",
            method : "POST"
            url : "http://mbuilder/app1/send",
            parameters : [
              { name : "foo", type : "string" },
              { name : "bar", type : "string" }
            ]
          })
        @actions.push(
          {
            action : "mbuilder app - contact user",
            method : "POST"
            url : "http://mbuilder/app1/contact",
            parameters : [
              { name : "lorem", type : "string" }
            ]
          }
        )

        @loading_actions(false)
      , 500

      @action.subscribe (newValue) =>
        # TODO keep session values and try to restore them if appropiate.
        @settings.removeAll()
        return unless newValue?
        for param in newValue.parameters
          @settings.push(new MbuilderStepSetting(@, name: param.name))
        console.log(@settings())

    default_name: () =>
      'Mbuilder'

    button_class: () =>
      'lpgear'

    to_hash: () =>
      $.extend(super,
        action: @action()
        settings: (setting.to_hash() for setting in @settings())
      )

