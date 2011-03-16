class HangupCommand < Command
  def run(session)
    session.pbx.hangup
  end
end
