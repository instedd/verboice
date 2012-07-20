class LocalizedResource < ActiveRecord::Base
  belongs_to :resource

  attr_accessible :audio, :language, :text, :type, :url
end
