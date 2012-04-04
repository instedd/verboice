class Commands::AnswerCommand < Command
  def run(session)
    session.info "Answer"
    session.pbx.answer
  end
end
