class PersistedVariable < ActiveRecord::Base
  has_one :account, :through => :contact
  belongs_to :contact

  attr_accessible :name, :value
  validates_presence_of :account, :contact, :name, :value
  attr_accessible :contact, :name, :value
end
