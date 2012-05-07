class RecordedAudio < ActiveRecord::Base
  belongs_to :contact
  belongs_to :call_log
  has_one :account, :through => :contact

  attr_accessible :description, :key
  validates_presence_of :call_log, :contact, :key, :description
end
