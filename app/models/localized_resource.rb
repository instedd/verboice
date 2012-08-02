class LocalizedResource < ActiveRecord::Base
  belongs_to :resource

  store :extras, accessors: [:duration, :description, :filename]

  attr_accessible :recorded_audio, :uploaded_audio, :language, :text, :type, :url, :description, :duration, :filename

  validates_presence_of :language #, :resource

  validates_uniqueness_of :language, :scope => :resource_id

  after_initialize do
    self.guid ||= Guid.new.to_s
  end

  def has_recorded_audio
    self.recorded_audio.present?
  end

  def has_uploded_audio
    self.uploaded_audio.present?
  end

  def as_json(options = {})
    super options.merge(:methods => [:type, :has_recorded_audio, :has_uploded_audio, :duration, :description, :filename],
      :except => [:recorded_audio, :uploaded_audio, :extras])
  end
end
