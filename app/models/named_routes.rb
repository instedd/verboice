class NamedRoutes
  include Singleton
  include Rails.application.routes.url_helpers

  def self.default_url_options
    Rails.application.config.default_url_options
  end

  def self.method_missing(method_sym, *arguments, &block)
    self.instance.send method_sym, *arguments, &block
  end
end