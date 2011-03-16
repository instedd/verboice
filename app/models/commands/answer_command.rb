class AnswerCommand < Command
  def run(session)
    session.pbx.answer
  end
end
