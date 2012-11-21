window.initProjectSettings = (languages, voices) ->
  class Language
    constructor: (model, value) ->
      @model = model
      @value = value
      @label = _.find(window.languages, (lang) -> lang.value == value).label
      @description = "#{@label} (#{@value})"
      @voice = ko.observable()

    voices: =>
      @model.voicesForLanguage(@value)

  class ProjectSettingsViewModel
    constructor: (languages, voices) ->
      @voices = voices
      @languages = ko.observableArray(_.map(languages, (value) => new Language(@, value)))
      @newLanguage = ko.observable('')
      @initAutocomplete()
      @hookToTtsEngine()
      @setIspeechOptionsVisibility()

    addLanguage: =>
      search = @newLanguage().toLowerCase()
      matches = _.select(window.languages, (lang) -> lang.label.toLowerCase().indexOf(search) >= 0)
      if matches.length == 1
        @addLanguageByValue matches[0].value

    addLanguageByValue: (value) =>
      @languages.push new Language(@, value) unless _.find(@languages(), (lang) -> lang.value == value)
      @newLanguage('')
      $('#add_language').autocomplete('close');

    removeLanguage: (language) =>
      @languages.remove language

    voicesForLanguage: (language) =>
      @voices[language]

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

    hookToTtsEngine: =>
      $("#project_tts_engine").change @ttsEngineChanged

    setIspeechOptionsVisibility: (speed) =>
      if $("#project_tts_engine").val() is "ispeech"
        $("#project_tts_ispeech_api_key").parent().show speed
      else
        $("#project_tts_ispeech_api_key").parent().hide speed

    ttsEngineChanged: =>
      @setIspeechOptionsVisibility('fast')
      $.get '/synthesizer/voices', {engine: $("#project_tts_engine").val()}, (voices) =>
        @voices = voices

  ko.applyBindings new ProjectSettingsViewModel(languages, voices)
