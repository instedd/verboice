#= require workflow/steps/step

onWorkflow ->
  class window.Language extends Step
    @type = 'language'

    constructor: (attrs) ->
      super(attrs)

      resource_hash_for = (key) =>
        _.find(attrs.languages, (l) => l.key == key)?.resource

      @next_id = attrs.next
      @languages = ({key: l.key, resource: new ResourceEditor(@, resource_hash_for(l.key))} for l in languages)
      @languages = ko.observable(@languages)

      @current_editing_resource = ko.observable(null)

      @is_editing_resource = ko.computed () =>
        @current_editing_resource() != null

      @is_invalid = ko.computed () =>
        @is_name_invalid() or not _.all((l.resource.is_valid() for l in @languages()), _.identity)

    button_class: () =>
      'llanguage'

    @add_to_steps: () ->
      workflow.add_step(new Language)

    @initialize: (hash) ->
      step = new Language(hash)
      return step

    to_hash: () =>
      $.extend(super,
        languages: ({key: l.key, resource: l.resource.to_hash()} for l in @languages())
      )

    default_name: () =>
      'Detect Language'

    show_resource: (language) =>
      @current_editing_resource(language.resource)

    title: (language) =>
      _.find(languages, (l) => l.key == language.key).value

    is_resource_invalid: (language) =>
      not language.resource.is_valid()

    number: (index) =>
      index() + 1
