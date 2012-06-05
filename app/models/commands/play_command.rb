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

module Commands::PlayCommand

  include AudioUtils

  def initialize(file_id)
    @file_id = file_id
  end

  def run(session)
    target_path = download session
    session.pbx.play target_path
    super
  end

  def download(session)
    target_path = get_target_path(session)
    if should_setup_file?(target_path)
      setup_file(session)
      session.trace "File #{target_path} prepared for playing"
    else
      session.trace "File #{target_path} already exists"
    end
    target_path
  end

  def get_target_path(session)
    @target_path ||= session.pbx.sound_path_for @file_id
  end

  def setup_file(session)
    raise "#{self.class.name} must implement setup_file"
  end

  def should_setup_file?(target_path)
    not File.exists? target_path
  end

end
