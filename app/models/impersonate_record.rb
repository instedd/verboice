class ImpersonateRecord < ActiveRecord::Base
  belongs_to :call_flow
  belongs_to :contact
  belongs_to :impersonated, class_name: 'Contact'
end
