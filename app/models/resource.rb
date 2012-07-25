class Resource < ActiveRecord::Base
  belongs_to :project
  has_many :localized_resources, :dependent => :destroy

  accepts_nested_attributes_for :localized_resources

  attr_accessible :name, :localized_resources_attributes

  validates_presence_of :name, :project
end
