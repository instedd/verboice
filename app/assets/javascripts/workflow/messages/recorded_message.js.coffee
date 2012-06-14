#= require workflow/messages/message

onWorkflow ->
  class window.RecordedMessage extends Message
    constructor: (hash={}) ->
      super(hash)
      @file = ko.observable hash.file
      @recording = ko.observable false
      @playing = ko.observable false
      @duration = ko.observable(hash.duration || (new Date).clearTime().toString('mm:ss'))
      @type = 'recording'
      @label = 'Record message'
      @recording_start = null
      @update_duration_interval = null

    record: () =>
      return if @playing() or @recording()
      @playing false
      @update_duration 0
      Wami.setup
        id: 'wami'
        swfUrl: '/Wami.swf'
        onReady: =>
          Wami.startRecording("#{save_recording_path}?#{@message_query_identifier()}",
            Wami.nameCallback(@wami_record_start), Wami.nameCallback(@wami_record_finished), Wami.nameCallback(@wami_record_failed));
          @recording_start = @now_seconds()
          @update_duration_interval = window.setInterval((() =>
            @update_duration(@now_seconds() - @recording_start)), 100)
      @alert_flash_required('recording')

    stop: () =>
      if Wami.stopRecording # check if Wami is loaded
        Wami.stopRecording() if @recording()
        Wami.stopPlaying() if @playing()
        @file(true)
      @playing(false)
      window.clearInterval(@update_duration_interval)

    play: () =>
      return if @playing() or @recording() or not @file()
      @recording(false)
      @playing(true)
      Wami.setup
        id: 'wami'
        swfUrl: '/Wami.swf'
        onReady: =>
          window.playFinished = () => @playing(false)
          url = "#{play_recording_path}?#{@message_query_identifier()}"
          Wami.startPlaying(url, null, Wami.nameCallback(window.playFinished))
      @alert_flash_required('playing')

    exit: () =>
      @stop()
      super

    to_hash: () =>
      if @file() or @name()?
        $.extend(super,
          file: @file()
          duration: @duration()
        )

    is_valid: () =>
      super() and @file()

    # private

    wami_record_start: () =>
      @recording(true)

    wami_record_finished: () =>
      @recording(false)

    wami_record_failed: () =>
      @recording(false)

    message_query_identifier: () =>
      return "step_id=#{@parent.id}&message=#{@title}"

    now_seconds: () =>
      Math.round(+new Date()/1000)

    update_duration: (seconds) =>
      @duration((new Date).clearTime().addSeconds(seconds).toString('mm:ss'))

    alert_flash_required: (action) =>
      if $('.flash-required').length
        $('.flash-required').html('')
        alert "Adobe Flash Player version 10.0.0 or higher is required for #{action} a message.\nDownload it from https://get.adobe.com/flashplayer/ and reload this page."
