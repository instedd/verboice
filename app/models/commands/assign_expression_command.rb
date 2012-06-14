class Commands::AssignExpressionCommand < Commands::AssignCommand

  def assign_data(session)
    session[@name.to_s] = session.eval @data
  end

end