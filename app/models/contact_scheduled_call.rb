class ContactScheduledCall < ActiveRecord::Base
  belongs_to :contact
  belongs_to :scheduled_call
end
