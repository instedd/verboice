class ContactsFinder
  def initialize(project)
    @project = project
  end

  def self.for(project)
    self.new(project)
  end

  def find(filters = [], options = {})
    contacts = @project.contacts.includes(options[:includes] || [])
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
      args << filter[:value].try_as_number
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
      if filter[:value].blank?
        "(vars.value = '' OR vars.value IS NULL)"
      else
        "vars.value = #{value}"
      end
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

    query = "EXISTS (SELECT 1 FROM persisted_variables as vars WHERE vars.contact_id = contacts.id AND #{variable} AND #{condition} LIMIT 1)"

    if filter[:operator].to_s == 'undefined' || (filter[:operator].to_s == 'eq' && filter[:value].blank?)
      defined_query, *defined_args = query_for(filter.merge(operator: 'defined'))
      return ["(#{query} OR NOT #{defined_query})"] + args + defined_args
    end

    [query] + args

  end

end
