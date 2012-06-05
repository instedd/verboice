class ExternalService < ActiveRecord::Base
  belongs_to :project
  has_many :external_service_steps, :autosave => true, :dependent => :destroy

  attr_accessible :name, :url, :xml, :global_variables_attributes

  serialize :global_settings, Hash

  def update_manifest!
    response = RestClient.get self.url
    self.xml = response.to_str
    self.update_from_manifest!(xml)
  end

  def steps
    external_service_steps
  end

  def update_from_manifest(xml)
    Parsers::ExternalService.new(self).parse(xml).save
  end

  def update_from_manifest!(xml)
    Parsers::ExternalService.new(self).parse(xml).save
  end

  def self.create_from_manifest(xml)
    Parsers::ExternalService.new.parse(xml).tap do |service|
      service.save
    end
  end

  def global_variables_attributes=(attributes)
    attributes.each do |index, attrs|
      variable = global_variables.detect{|v| v.name == attrs[:name]}
      variable.value = attrs[:value] if variable
    end
  end

  def global_variable_value_for(name)
    global_variables.detect{|v| v.name == name}.try(:value)
  end

  def global_variables
    global_settings[:variables] ||= []
  end

  def global_variables=(vars)
    global_settings[:variables] = vars
  end

  class GlobalVariable
    attr_accessor :name, :display_name, :type, :value

    def initialize(opts = {})
      @name = opts[:name]
      @display_name = opts[:display_name]
      @type = opts[:type]
      @value = opts[:value]
    end

    def persisted?
      false
    end
  end

end
