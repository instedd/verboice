class Commands::RetrieveVariableCommand < Command

  def initialize variable_name
    @variable_name = variable_name
  end

  def run session
    account = session.call_log.account
    contact = account.contacts.find_by_address(session.address)
    if contact
      persisted_variable = contact.persisted_variables.find_by_name @variable_name
      Commands::AssignCommand.new("var_#{@variable_name}", persisted_variable.value).run(session) if persisted_variable
    end
    super
  end
end