class LocalizedResource < ActiveRecord::Base
  belongs_to :resource

  store :extras, accessors: [:duration, :description]

  attr_accessible :audio, :language, :text, :type, :url, :description, :duration

  validates_presence_of :language #, :resource

  def has_audio
    self.audio.present?
  end

  def as_json(options = {})
    super options.merge(:methods => [:type, :has_audio, :duration, :description], :except => [:audio, :extras])
  end
end
