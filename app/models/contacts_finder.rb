class ContactsFinder
  def initialize(project)
    @project = project
  end

  def self.for(project)
    self.new(project)
  end

  def find(filters = [], options = {})
    includes = (options[:includes] || [])
    contacts = @project.contacts.includes(includes).joins(:persisted_variables)
    filters.inject contacts do |contacts, filter|
      contacts.where(*query_for(filter.with_indifferent_access))
    end
  end

private

  def query_for(filter)
    args = []

    variable = if filter[:project_variable_id].present?
      args << filter[:project_variable_id]
      "vars.project_variable_id = ?"
    elsif filter[:implicit_key].present?
      args << filter[:implicit_key]
      "vars.implicit_key = ?"
    else
      "FALSE"
    end

    value = if filter[:value].present?
      args << filter[:value]
      "?"
    elsif filter[:other_project_variable_id].present?
      args << filter[:other_project_variable_id]
      "(SELECT value FROM persisted_variables as other_vars WHERE other_vars.contact_id = contacts.id AND other_vars.project_variable_id = ? LIMIT 1)"
    elsif filter[:other_implicit_key].present?
      args << filter[:other_implicit_key]
      "(SELECT value FROM persisted_variables as other_vars WHERE other_vars.contact_id = contacts.id AND other_vars.implicit_key = ? LIMIT 1)"
    else
      "FALSE"
    end

    condition = case filter[:operator].to_s
    when "eq"
      "vars.value = #{value}"
    when "geq"
      "vars.value >= #{value}"
    when "gt"
      "vars.value > #{value}"
    when "leq"
      "vars.value <= #{value}"
    when "lt"
      "vars.value < #{value}"
    when "defined"
      "vars.value IS NOT NULL"
    when "undefined"
      "vars.value IS NULL"
    when "includes"
      "vars.value LIKE ('%' ? '%')"
    else
      "FALSE"
    end

    return ["EXISTS (SELECT 1 FROM persisted_variables as vars WHERE vars.contact_id = contacts.id AND #{variable} AND #{condition} LIMIT 1)"] + args
  end

  def matches(contact, filter)
    variable = contact.persisted_variables.find do |v|
      if filter[:project_variable_id].present?
        v.project_variable_id == filter[:project_variable_id]
      else
        v.implicit_key == filter[:implicit_key]
      end
    end

    value = if filter[:value].present?
      filter[:value]
    elsif filter[:other_project_variable_id].present?
      other_variable = contact.persisted_variables.find{|v| v.project_variable_id == filter[:other_project_variable_id]}
      other_variable.try(:value)
    elsif filter[:other_implicit_key].present?
      other_variable = contact.persisted_variables.find{|v| v.implicit_key == filter[:other_implicit_key]}
      other_variable.try(:value)
    else
      nil
    end

    case filter[:operator].try(:to_sym)
    when :eq
      variable.present? && variable.value == value.try(:to_s)
    when :geq
      variable.present? && variable.is_number? && variable.typecasted_value >= value.try(:to_i)
    when :gt
      variable.present? && variable.is_number? && variable.typecasted_value > value.try(:to_i)
    when :leq
      variable.present? && variable.is_number? && variable.typecasted_value <= value.try(:to_i)
    when :lt
      variable.present? && variable.is_number? && variable.typecasted_value < value.try(:to_i)
    when :defined
      variable.present? && !variable.value.nil?
    when :undefined
      variable.nil? || variable.value.nil?
    when :includes
      variable.present? && !value.nil? && !variable.value.nil? && variable.value.include?(value)
    else
      false
    end
  end

end
