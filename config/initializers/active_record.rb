class ActiveRecord::Base
  def self.config_accessor(*names)
    options = names.extract_options!
    default = options[:default]

    names.each do |name|
      define_method(name) do
        config[name.to_s] || default
      end
      define_method("#{name}=") do |value|
        config_will_change!
        config[name.to_s] = value
      end
    end
  end
end
