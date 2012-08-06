class Resource < ActiveRecord::Base
  self.primary_key = 'guid'

  belongs_to :project
  has_many :localized_resources, :dependent => :destroy, :foreign_key => :resource_guid

  accepts_nested_attributes_for :localized_resources

  attr_accessible :name, :localized_resources_attributes

  validates_presence_of :name, :project

  after_initialize do
    self.guid ||= Guid.new.to_s
  end

  def as_json(options = {})
    # Fix for rails not calling as_json on includes, remove when fixed
    include_localized_resources = false
    if options[:include] == :localized_resources
      options.delete :include
      include_localized_resources = true
    end
    result = super
    result[:localized_resources] = self.localized_resources.as_json if include_localized_resources
    result
  end

  def available_resource_for language
    resource = resource_for language
    resource = resource_for(project.default_language) unless resource.present?
    resource
  end

  def resource_for language
    localized_resources.detect do |localized_resource|
      localized_resource.language == language
    end
  end
end
