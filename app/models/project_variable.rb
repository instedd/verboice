class ProjectVariable < ActiveRecord::Base
  belongs_to :project, :inverse_of => :project_variables
  has_many :persisted_variables, :dependent => :destroy, :inverse_of => :project_variable
  attr_accessible :name
  validates_uniqueness_of :name, :scope => :project_id

end