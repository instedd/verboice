#= require workflow/steps/step

onWorkflow ->
  class window.Language extends Step
    @type = 'language'

    constructor: (attrs) ->
      super(attrs)

      @languages = project_languages

      @next_id = attrs.next
      @resource = new ResourceEditor(@, attrs.resource)

      @current_editing_resource = ko.observable(null)
      @is_editing_resource = ko.computed () =>
        @current_editing_resource() != null

      @is_invalid = ko.computed () =>
        @is_name_invalid() or not @resource.is_valid()

      @force_question = ko.observable(attrs.force_question)

    button_class: () =>
      'llanguage'

    @add_to_steps: () ->
      workflow.add_step(new Language)

    @initialize: (hash) ->
      step = new Language(hash)
      return step

    to_hash: () =>
      $.extend(super,
        resource: @resource.to_hash(),
        force_question: @force_question()
      )

    default_name: () =>
      'Detect Language'

    show_language: (language) =>
      @current_editing_resource(@resource)
      @resource.show_language(language.key)

    number: (index) =>
      index() + 1
