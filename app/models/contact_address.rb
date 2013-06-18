class ContactAddress < ActiveRecord::Base
  belongs_to :contact, :inverse_of => :addresses
  belongs_to :project, :inverse_of => :contact_addresses

  attr_accessible :address

  validates_presence_of :address, :contact, :project
  validates_uniqueness_of :address, :scope => :project_id

  before_validation :copy_project_id

  private

  def copy_project_id
    self.project_id = self.contact.project_id unless self.contact.nil?
    true
  end
end
