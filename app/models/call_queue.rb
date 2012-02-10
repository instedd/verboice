class CallQueue < ActiveRecord::Base
  belongs_to :account
  has_many :queued_calls

  validates_presence_of :account
  validates_presence_of :name
  validates_uniqueness_of :name, :case_sensitive => false, :scoped_to => :account_id
  validates_format_of :retries, :with => /^[0-9\.]+(,[0-9\.]+)*$/, :allow_blank => true
end
