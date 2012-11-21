window.initProjectSettings = (languages) ->
  class Language
    constructor: (value) ->
      @value = value
      @label = _.find(window.languages, (lang) -> lang.value == value).label

    description: =>
      "#{@label} (#{@value})"

  class ProjectSettingsViewModel
    constructor: (languages) ->
      @languages = ko.observableArray(_.map(languages, (value) -> new Language(value)))
      @newLanguage = ko.observable('')
      @initAutocomplete()

    addLanguage: =>
      search = @newLanguage().toLowerCase()
      matches = _.select(window.languages, (lang) -> lang.label.toLowerCase().indexOf(search) >= 0)
      if matches.length == 1
        @addLanguageByValue matches[0].value

    addLanguageByValue: (value) =>
      @languages.push new Language(value)
      @newLanguage('')
      $('#add_language').autocomplete('close');

    removeLanguage: (language) =>
      @languages.remove language

    keydown: =>
      if event.keyCode == 13
        @addLanguage()
        false
      else
        true

    initAutocomplete: =>
      $('#add_language').autocomplete
        source: window.languages
        select: (event, ui) =>
          unless _.find(@languages(), (lang) -> lang.value == ui.item.value)
            @addLanguageByValue ui.item.value
          false

  ko.applyBindings new ProjectSettingsViewModel(languages)

  set_ispeech_options_visibility = (speed) ->
    if $("#project_tts_engine").val() is "ispeech"
      $("#project_tts_ispeech_api_key").parent().show speed
    else
      $("#project_tts_ispeech_api_key").parent().hide speed

  $("#project_tts_engine").change ->
    set_ispeech_options_visibility('fast')

  set_ispeech_options_visibility()
