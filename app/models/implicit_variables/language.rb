module ImplicitVariables
  class Language < ImplicitVariable

    def value(use_default = true)
      persisted_variable = @contact.persisted_variables.find_by_implicit_key(self.class.key)
      if persisted_variable
        persisted_variable.typecasted_value
      elsif use_default
        @contact.project.default_language
      else
        nil
      end
    end

    def self.key
      'language'
    end

  end
end
