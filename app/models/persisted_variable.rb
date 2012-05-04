class PersistedVariable < ActiveRecord::Base
  has_one :account, :through => :contact
  belongs_to :contact

  validates_presence_of :account, :contact, :name, :value
  attr_accessible :contact, :name, :value
end
