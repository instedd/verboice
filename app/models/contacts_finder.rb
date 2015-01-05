class ContactsFinder
  def initialize(project)
    @project = project
  end

  def self.for(project)
    self.new(project)
  end

  def find(filters = [], options = {})
    includes = (options[:includes] || []) | [:persisted_variables]
    contacts = @project.contacts.includes(includes)
    if filters.size > 0
      filters = filters.map(&:with_indifferent_access)
      contacts.select do |contact|
        filters.map do |filter|
          matches contact, filter
        end.all?
      end
    else
      contacts
    end
  end

private

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
      variable.present? && variable.typecasted_value >= value.try(:to_i)
    when :gt
      variable.present? && variable.typecasted_value > value.try(:to_i)
    when :leq
      variable.present? && variable.typecasted_value <= value.try(:to_i)
    when :lt
      variable.present? && variable.typecasted_value < value.try(:to_i)
    when :defined
      variable.present? && !variable.value.nil?
    when :undefined
      variable.nil? || variable.value.nil?
    when :includes
      variable.present? && !value.nil? && variable.value.include?(value)
    else
      false
    end
  end

end
