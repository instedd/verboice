class PersistedVariable < ActiveRecord::Base
  belongs_to :account
  attr_accessible :address, :name, :value
  validates_presence_of :account, :address, :name, :value

end
