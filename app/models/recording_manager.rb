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

class RecordingManager

  def initialize(object)
    @object = object
  end

  def self.for(object)
    raise "Cannot create recording manager for non saved object" unless object.id
    self.new(object)
  end

  def save_recording_for(key)
    path = recording_path_for(key)
    File.open(path,"wb") do |file|
      yield(file)
    end
  end

  def recording_path_for(key)
    File.join(recordings_folder, "#{key}.wav")
  end

  def result_path_for(key)
    File.join(results_folder, "#{key}.wav")
  end

  def results_folder
    path_for 'results'
  end

  def recordings_folder
    path_for 'recordings'
  end

  def path_for(folder)
    path = File.join Rails.root, "data", "#{@object.class.name.underscore.pluralize}", "#{@object.id}", "#{folder}"
    FileUtils.makedirs(path)
    path
  end

  def self.format_recording(id, action)
    "#{id}-#{action.to_s.parameterize}"
  end

end
