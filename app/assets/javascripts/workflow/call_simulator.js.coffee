onWorkflow ->
  class window.CallSimulator
    constructor: (@workflow) ->
      @display = ko.observable()
      @duration = ko.observable(0)
      @duration_text = ko.computed => "Call duration: #{@toHHMMSS(@duration())}"

    display_template_id: () ->
      'call_simulator_template'

    start: (workflow) =>
      serialized = @workflow.serialize()

      @duration(0)
      @duration_timer = setInterval((=> @duration(@duration() + 1)), 1000)

      $.post "/call_simulator/start", {call_flow_id: window.callFlowId, flow: serialized}, @callback

    callback: (response) =>
      @workflow.current_step @workflow.get_step(response.current_step)
      switch response.command
        when 'say'
          @display(response.text)

          audio = $('#call-simulator-audio')[0]
          audio.src = response.path
          audio.play()

          # audio.onended =>
          #   $.post "/call_simulator/resume", {session_id: response.session_id}, @callback
        when 'capture'
          console.log response
          @display(response.say) if response.say
          @capture = response
          @digits = ''
          @reset_capture_timer()
        when 'hangup'
          @display('Call ended...')
          setTimeout @workflow.call_simulator_ended, 3000
          clearInterval @duration_timer

    digit_pressed: (model, event) =>
      return unless @capture

      digit = event.target.textContent
      is_finish_key = digit == @capture.finish_on_key

      if is_finish_key && @digits.length == 0
        @post_finish_key()
        return

      @digits += digit unless is_finish_key
      @display(@digits)

      @reset_capture_timer()

      if @digits.length == @capture.max || is_finish_key
        @post_digits()

    reset_capture_timer: =>
      clearInterval(@capture_timer) if @capture_timer
      @capture_timer = setTimeout(@capture_time_expired, 1000 * @capture.timeout) if @capture.timeout

    clear_capture_timer: =>
      clearInterval(@capture_timer) if @capture_timer
      @capture_timer = null

    capture_time_expired: =>
      @post_digits()

    post_digits: =>
      @clear_capture_timer()
      data = if @digits.length < @capture.min then 'timeout' else @digits
      $.post "/call_simulator/resume", {session_id: @capture.session_id, data: data}, @callback
      @capture = null

    post_finish_key: =>
      @clear_capture_timer()
      $.post "/call_simulator/resume", {session_id: @capture.session_id, data: 'finish_key'}, @callback
      @capture = null

    toHHMMSS: (seconds) ->
      minutes = Math.floor(seconds / 60)
      seconds = seconds - (minutes * 60)
      minutes = "0" + minutes  if minutes < 10
      seconds = "0" + seconds  if seconds < 10
      minutes + ":" + seconds

