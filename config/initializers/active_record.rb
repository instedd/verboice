class ActiveRecord::Base
  def self.config_accessor(*names)
    options = names.extract_options!
    default = options[:default]

    names.each do |name|
      define_method(name) do
        config.try(:[], name.to_s) || default
      end

      define_method("#{name}=") do |value|
        respond_to?(:encrypted_config_will_change!) ? encrypted_config_will_change! : config_will_change!

        self.config ||= {}
        self.config[name.to_s] = value

        # this is needed so attr_encrypted encrypts the value correctly
        self.config = config
      end
    end
  end
end
