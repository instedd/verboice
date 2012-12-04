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
          setTimeout (=> $.post "/call_simulator/resume", {session_id: response.session_id}, @callback), 3000
        when 'capture'
          @display(response.say) if response.say
          @capture = response
          @digits = ''
        when 'hangup'
          @display('Call ended...')
          setTimeout @workflow.call_simulator_ended, 3000
          clearInterval @duration_timer

    digit_pressed: (model, event) =>
      return unless @capture

      digit = event.target.textContent
      @digits += digit
      @display(@digits)

      if @digits.length == @capture.max || digit == @capture.finish_on_key
        $.post "/call_simulator/resume", {session_id: @capture.session_id, digits: @digits}, @callback
        @capture = null

    toHHMMSS: (seconds) ->
      minutes = Math.floor(seconds / 60)
      seconds = seconds - (minutes * 60)
      minutes = "0" + minutes  if minutes < 10
      seconds = "0" + seconds  if seconds < 10
      minutes + ":" + seconds

