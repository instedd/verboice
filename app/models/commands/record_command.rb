class RecordCommand < Command
  def run(session)
    session.info "Record user voice"
    session.pbx.record
  end
end
