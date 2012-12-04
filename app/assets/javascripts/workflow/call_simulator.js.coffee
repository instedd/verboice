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
      switch response.command
        when 'say'
          @display(response.text)
          setTimeout (=> $.post "/call_simulator/resume", {session_id: response.session_id}, @callback), 3000
        when 'hangup'
          @display('Call ended...')
          setTimeout @workflow.call_simulator_ended, 3000

    digit_pressed: (model, event) =>
      digit = event.target.textContent
