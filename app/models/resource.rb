class Resource < ActiveRecord::Base
  belongs_to :project
  has_many :localized_resources

  attr_accessible :name

  validates_presence_of :name
end
