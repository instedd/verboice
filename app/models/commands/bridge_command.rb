class Commands::BridgeCommand

  def initialize(session_id)
    @session_id = session_id.to_i
  end

  def run(session)
    session.info "Bridging with session #{@session_id}"
    other_session = BaseBroker.instance.find_session_by_call_log_id @session_id
    session.pbx.bridge_with other_session
  end

end