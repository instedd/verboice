class Resource < ActiveRecord::Base
  belongs_to :project
  has_many :localized_resources, :dependent => :destroy

  accepts_nested_attributes_for :localized_resources

  attr_accessible :name, :localized_resources_attributes

  validates_presence_of :name, :project

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
end
