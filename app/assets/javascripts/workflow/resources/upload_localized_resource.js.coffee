#= require workflow/resources/localized_resource

onWorkflow ->
  class window.UploadLocalizedResource extends LocalizedResource

    constructor: (hash = {}) ->
      super(hash)

      @label = 'Upload a file'
      @template = 'upload_localized_resource_template'

      @url = ko.observable "#{save_recording_path}?#{@message_query_identifier()}"
      @description = ko.observable null

    message_query_identifier: () =>
      "step_id=#{1}&message=#{2}"
