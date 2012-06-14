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

class Commands::WhileCommand < Command
  attr_accessor :condition
  attr_accessor :block

  def initialize(condition, block)
    @condition = condition
    @block = block
    if block
      block.last.next = self
    else
      @block = self
    end
  end

  def run(session)
    session.trace "Testing statement: #{@condition}.", command: 'while', action: 'testing'
    if session.eval @condition
      session.trace "The statement is true. running cycle.", command: 'while', action: 'true'
      @block
    else
    session.trace "The statement is false. Ending while.", command: 'while', action: 'false'
      super
    end
  end
end
