#= require workflow/resources/localized_resource

onWorkflow ->
  class window.UploadLocalizedResource extends LocalizedResource

    constructor: (hash = {}) ->
      super(hash)

      @label = 'Upload a file'
      @template = 'upload_localized_resource_template'

      @description = ko.observable hash.description
      @has_audio = ko.observable hash.has_uploaded_audio
      @filename = ko.observable hash.filename
      @url = ko.computed =>
        if @is_saved()
          "/projects/#{project_id}/resources/#{@parent().id()}/localized_resources/#{@id()}/save_file?filename=#{@filename()}"
        else
          null

      @is_valid = ko.computed =>
        @has_audio()

    to_hash: () =>
      $.extend(super,
        description: @description(),
        filename: @filename()
      )

    type: () =>
      'UploadLocalizedResource'

    download: () =>
      downloadURL "/projects/#{project_id}/resources/#{@parent().id()}/localized_resources/#{@id()}/play_file"

    # fileupload callbacks
    add: (e, data) =>
      @filename(data.files[0].name)
      data.url = @url()

    submit: () =>
      unless @is_saved()
        alert 'Please save this message before uploading a file'
        return false

    done: () =>
      @has_audio(true)
