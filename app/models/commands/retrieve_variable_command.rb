class Commands::RetrieveVariableCommand < Command

  attr_accessor :variable_name

  def initialize variable_name
    @variable_name = variable_name
  end

  def run session
    account = session.call_log.account
    contact = account.contacts.find_by_address(session.address)
    if contact
      persisted_variable = contact.persisted_variables.find_by_name @variable_name
      if persisted_variable
        Commands::AssignCommand.new("var_#{@variable_name}", persisted_variable.value).run(session)
      else
        set_to_nil session
      end
    else
      set_to_nil session
    end
    super
  end

  def set_to_nil session
    Commands::AssignCommand.new("var_#{@variable_name}", nil).run(session)
  end
end