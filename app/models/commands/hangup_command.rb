class Commands::HangupCommand < Command
  def run(session)
    session.info 'Hangup'
    session.pbx.hangup
    super
  end
end
