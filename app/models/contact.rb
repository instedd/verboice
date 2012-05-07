class Contact < ActiveRecord::Base
  belongs_to :account
  has_many :persisted_variables, :dependent => :destroy
  has_many :recorded_audios, :dependent => :destroy

  attr_accessible :address
  validates_presence_of :account, :address
end
