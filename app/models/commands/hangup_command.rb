class HangupCommand
  def run(session)
    session.pbx.hangup
  end
end
