class NamedRoutes
  include Singleton
  include Rails.application.routes.url_helpers

  DefaultUrlOptions = Rails.configuration.verboice_configuration[:default_url_options].to_hash.symbolize_keys!

  def self.default_url_options
    DefaultUrlOptions
  end

  def self.method_missing(method_sym, *arguments, &block)
    self.instance.send method_sym, *arguments, &block
  end
end