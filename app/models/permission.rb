class Permission < ActiveRecord::Base
  attr_accessible :account_id, :flags, :model_id, :type
  belongs_to :account

end
