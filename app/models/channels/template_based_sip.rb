class Channels::TemplateBasedSip < Channels::Sip

  config_accessor :kind
  config_accessor :server_url

  class<<self
    attr_reader :templates
  end

  def servers
    [Server.new(server_url, true, 'both')]
  end

  def self.kinds
    templates.inject [] do |kinds, template|
      kinds << ["#{template[0]} channel", "#{name}-#{template[0]}"]
      kinds
    end
  end

  def limit
    1
  end

  def register?
    true
  end

  def self.can_handle? a_kind
    templates.keys.any? do |template_name|
      template_name.downcase == a_kind
    end
  end

  @templates = YAML::load_file("#{Rails.root}/config/sip_channel_templates.yml").with_indifferent_access

  templates.each do |template_name, server_url|
    define_singleton_method "new_#{template_name.underscore}_channel" do
      template = Channels::TemplateBasedSip.new
      template.kind = template_name
      template.server_url = server_url
      template
    end
  end
end