#= require workflow/steps/step

onWorkflow ->
  class window.Language extends Step
    @type = 'language'

    constructor: (attrs) ->
      super(attrs)

      message_for = (key) =>
        _.find(attrs.languages, (l) => l.key == key)?.message

      @next_id = attrs.next
      @languages = ({key: l.key, message: MessageSelector.from_hash(message_for(l.key)).with_title(l.value).with_parent(@)} for l in languages)
      @languages = ko.observable(@languages)

      @current_editing_message = ko.observable(null)

      @is_editing_message = ko.computed () =>
        @current_editing_message() != null

      @is_invalid = ko.computed () =>
        @is_name_invalid() or not _.all((l.message.is_valid() for l in @languages()), _.identity)

    button_class: () =>
      'llanguage'

    @add_to_steps: () ->
      workflow.add_step(new Language)

    @initialize: (hash) ->
      step = new Language(hash)
      return step

    to_hash: () =>
      $.extend(super,
        languages: ({key: l.key, message: l.message.to_hash()} for l in @languages())
      )

    default_name: () =>
      'Detect Language'

    show_message: (language) =>
      @current_editing_message(language.message)

    title: (language) =>
      _.find(languages, (l) => l.key == language.key).value

    is_message_invalid: (language) =>
      not language.message.is_valid()

    number: (index) =>
      index() + 1
