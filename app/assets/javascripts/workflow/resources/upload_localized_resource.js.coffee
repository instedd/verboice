#= require workflow/resources/localized_resource

onWorkflow ->
  class window.UploadLocalizedResource extends LocalizedResource

    constructor: (hash = {}) ->
      super(hash)

      @label = 'Upload a file'
      @template = 'upload_localized_resource_template'

      @description = ko.observable hash.description
      @has_audio = ko.observable hash.has_audio
      @url = ko.computed =>
        if @is_saved()
          "/projects/#{project_id}/resources/#{@parent().id()}/localized_resources/#{@id()}/save_recording"
        else
          null

    to_hash: () =>
      $.extend(super,
        description: @description()
      )

    submit: () =>
      unless @is_saved()
        alert 'Please save this message before uploading a file'
        return false
