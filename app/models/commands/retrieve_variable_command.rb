class Commands::RetrieveVariableCommand < Command

  attr_accessor :variable_name

  def initialize variable_name
    @variable_name = variable_name
  end

  def run session
    account = session.call_log.account
    contact = account.contacts.find_by_address(session.address.presence || "Anonymous#{session.call_log.id}" )

    if contact && (persisted_variable = contact.persisted_variables.find_by_name @variable_name)
        set_value_to persisted_variable.value, session
      unless session.address.presence
        session.trace "Caller address is unknown. For current call, variable '#{@variable_name}' has been retrieved from contact '#{contact.address}'."
      end
    else
      set_value_to_nil_for session
      unless session.address.presence
        session.trace "Caller address is unknown. Variable '#{@variable_name}' can't be retrieved for an anonymous contact."
      end
    end
    super
  end

  def set_value_to_nil_for session
    set_value_to nil, session
  end

  def set_value_to value, session
    Commands::AssignCommand.new("var_#{@variable_name}", value).run(session)
  end
end