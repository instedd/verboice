class CallQueue < ActiveRecord::Base
  belongs_to :account

  validates_presence_of :account
  validates_presence_of :name
  validates_uniqueness_of :name, :case_sensitive => false, :scoped_to => :account_id
end
