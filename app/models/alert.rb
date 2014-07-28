class Alert < ActiveRecord::Base
  belongs_to :account
  serialize :data, Hash
end
