onWorkflow ->
  class window.CallSimulator
    constructor: (@workflow) ->
      @display = ko.observable()

    display_template_id: () ->
      'call_simulator_template'

    start: (workflow) =>
      serialized = @workflow.serialize()
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

    digit_pressed: (model, event) =>
      return unless @capture

      digit = event.target.textContent
      @digits += digit
      @display(@digits)

      if @digits.length == @capture.max || digit == @capture.finish_on_key
        $.post "/call_simulator/resume", {session_id: @capture.session_id, digits: @digits}, @callback
        @capture = null

