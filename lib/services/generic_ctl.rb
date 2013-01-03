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

def run(script, id = nil)
  require 'bundler/setup'
  require 'rubygems'
  require 'daemons'
  app_name = script
  app_name += ".#{id}" if id
  Daemons.run(
    File.join(File.dirname(__FILE__), "#{script}.rb"),
    :app_name => "verboice_#{app_name}",
    :dir_mode => :normal,
    :dir => File.join(File.dirname(__FILE__), '..', '..', 'tmp', 'pids'),
    :backtrace => true
  )
end
