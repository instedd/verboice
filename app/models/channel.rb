class Channel < ActiveRecord::Base
  belongs_to :account
  belongs_to :application

  validates_presence_of :account
  validates_presence_of :application
end
