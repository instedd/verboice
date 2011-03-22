class Rails::Application::Configuration
  def method_missing(name, *args)
    if name =~ /_configuration$/
      @configs ||= {}
      @configs[$`] ||= YAML::load_file("#{Rails.root}/config/#{$`}.yml").with_indifferent_access
    else
      super
    end
  end
end
