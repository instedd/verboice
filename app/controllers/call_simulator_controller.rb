class CallSimulatorController < ApplicationController
  before_filter :authenticate_account!

  $call_simulator_sessions ||= {}

  def start
    call_flow = CallFlow.find params[:call_flow_id]
    user_flow = JSON.parse params[:flow]

    parser  = Parsers::UserFlow.new call_flow, user_flow
    flow = parser.equivalent_flow

    session = Session.new trace_enabled: false
    session.call_flow = call_flow
    session.call_log = VirtualCallLog.new(call_flow)
    session.channel = VirtualChannel.new
    session.pbx = VirtualPbx.new
    session.commands = flow

    fiber = Fiber.new do
      session.run
      {command: :hangup}
    end

    $call_simulator_sessions[session.id] = [session, fiber]

    reply_command session.id
  end

  def resume
    reply_command params[:session_id], params[:data]
  end

  def reply_command(session_id, data = nil)
    session, fiber = $call_simulator_sessions[session_id]

    command = fiber.resume data

    if command[:command] == :hangup
      $call_simulator_sessions.delete session_id
    end

    render json: command.merge(session_id: session_id, current_step: session['current_step'])
  end

  class VirtualPbx
    def answer
      puts "ANSWER"
    end

    def hangup
      puts "HANGUP"
    end

    def say(text, options = {})
      puts "SAY: #{text}"
      Fiber.yield command: :say, text: text
    end

    def capture(options = {})
      Fiber.yield options.merge(command: :capture)
    end
  end

  class VirtualCallLog
    def initialize(call_flow)
      @call_flow = call_flow
    end

    def outgoing?
      false
    end

    def project
      @call_flow.project
    end

    def address
      'browser'
    end

    CallLogEntry::Levels.each do | severity |
      class_eval %Q(
        def #{severity}(description, options = {})
        end
      )
    end
  end

  class VirtualChannel
  end
end