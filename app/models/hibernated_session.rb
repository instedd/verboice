class HibernatedSession < ActiveRecord::Base
  attr_accessible :data, :session_id
end
