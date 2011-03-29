class Channel < ActiveRecord::Base
  belongs_to :account
  belongs_to :application
end
