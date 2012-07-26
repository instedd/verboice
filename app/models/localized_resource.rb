class LocalizedResource < ActiveRecord::Base
  belongs_to :resource

  attr_accessible :audio, :language, :text, :type, :url

  validates_presence_of :language #, :resource
end
