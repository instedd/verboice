class LocalizedResource < ActiveRecord::Base
  belongs_to :resource

  attr_accessible :audio, :language, :text, :type, :url

  validates_presence_of :language #, :resource

  def has_audio
    self.audio.present?
  end

  def as_json(options = {})
    super options.merge(:methods => [:type, :has_audio], :except => :audio)
  end
end
