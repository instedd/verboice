class Commands::PersistVariableCommand < Command

  attr_accessor :variable_name, :expression

  def initialize variable_name, expression
    @variable_name = variable_name
    @expression = expression
  end

  def run session
    account = session.call_log.account
    contact = account.contacts.find_by_address(session.address)
    contact = account.contacts.create! address: session.address unless contact

    contact.persisted_variables.create!\
      name: @variable_name,
      value: session.eval(@expression)
    super
  end
end