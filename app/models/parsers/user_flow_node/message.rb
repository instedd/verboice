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

class Parsers::UserFlowNode::Message

  def self.can_handle? params
    subclass_responsibility
  end

  def self.for call_flow, parent_step, action, params
    (SuitableClassFinder.find_direct_subclass_of self, suitable_for: (params || {})).new call_flow, parent_step, action, (params || {})
  end

  def name
    subclass_responsibility
  end

  def equivalent_flow
    subclass_responsibility
  end

  def capture_flow
    subclass_responsibility
  end
end