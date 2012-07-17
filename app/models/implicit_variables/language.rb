module ImplicitVariables
  class Language < ImplicitVariable

    def value
      persisted_variable = @contact.persisted_variables.find_by_implicit_key(self.class.key)
      if persisted_variable
        persisted_variable.typecasted_value
      else
        @contact.project.default_language
      end
    end

    def self.key
      'language'
    end

  end
end
