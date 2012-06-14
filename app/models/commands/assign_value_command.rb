class Commands::AssignValueCommand < Commands::AssignCommand

  def assign_data(session)
    session[@name.to_s] = @data
  end

end