class Commands::PersistVariableCommand < Command

  attr_accessor :variable_name, :expression

  def initialize variable_name, expression
    @variable_name = variable_name
    @expression    = expression
  end

  def run session
    contact            = contact_from session
    persisted_variable = contact.persisted_variables.find_by_name @variable_name

    if persisted_variable
      persisted_variable.value = evaluate_expression(session)
      persisted_variable.save!
    else
      contact.persisted_variables.create!\
        name: @variable_name,
        value: evaluate_expression(session)
    end
    super
  end

  def evaluate_expression(session)
    if @expression
      session.eval(@expression)
    else
      nil
    end
  end

  def contact_from session

    account = session.call_log.account

    contact = if session.address.presence
      account.contacts.where(:address => session.address).first_or_create!
    else
      account.contacts.where(:address => "Anonymous#{session.call_log.id}", :anonymous => true).first_or_create!
    end
    session.trace "Caller address is unknown. Variable '#{@variable_name}' saved for contact #{contact.address}." unless session.address.presence

    contact
  end
end