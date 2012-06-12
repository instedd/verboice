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

class Commands::AssignCommand < Command
  attr_accessor :name
  attr_accessor :expr
  attr_accessor :try

  def initialize(name, expr, try=nil)
    @name = name
    @expr = expr
    @try = try
  end

  def run(session)
    session.trace "Assign: #{@name}", command: 'assign', action: 'start'
    begin
      session[@name.to_s] = session.eval @expr
    rescue
      raise unless @try
    end
    session.trace "Assign: #{@name}", command: 'assign', action: 'finish'
    super
  end
end
