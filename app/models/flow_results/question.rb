# Copyright (C) 2010-2017, InSTEDD
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
class FlowResults::Question
  attr_accessor :type
  attr_accessor :label

  def initialize(type, label)
    @type = type
    @label = label
  end

  # Step is an item of the user_flow as serialized to the CallFlow table
  def self.from_step(step)
    case step["type"]
    when "capture"
      FlowResults::Question::Numeric.new(step["name"])
    when "menu"
      FlowResults::Question::SelectOne.new(step["name"], step["options"].map{|o| o["number"].to_s})
    else
      nil
    end
  end
end

class FlowResults::Question::Numeric < FlowResults::Question
  def initialize(label)
    super(:numeric, label)
  end
end

class FlowResults::Question::SelectOne < FlowResults::Question
  attr_accessor :choices

  def initialize(label, choices)
    super(:select_one, label)
    @choices = choices
  end
end