# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

class LocalizedResource < ActiveRecord::Base

  belongs_to :resource
  has_one :project, through: :resource
  store :extras, accessors: [:duration, :description, :filename]
  attr_accessible :encoded_audio, :recorded_audio, :uploaded_audio, :language, :text, :type, :url, :description, :duration, :filename, :extras
  validates_presence_of :language #, :resource
  validates_uniqueness_of :language, :scope => :resource_id
  validates :guid, :presence => true, :uniqueness => { :scope => :resource_id }

  broker_cached

  after_initialize do
    self.guid ||= Guid.new.to_s
  end

  def has_recorded_audio
    self.recorded_audio.present?
  end

  def has_uploaded_audio
    self.uploaded_audio.present?
  end

  def as_json options = {}
    super options.merge(:methods => [:type, :has_recorded_audio, :has_uploaded_audio, :duration, :description, :filename],
      :except => [:recorded_audio, :uploaded_audio, :extras])
  end

  def play_command_for play_resource_command
    subclass_responsibility
  end

  def capture_resource_for play_resource_command, session
    subclass_responsibility
  end

  def encoded_audio= encoded_audio
    self.recorded_audio = Base64.decode64 encoded_audio
  end
end
