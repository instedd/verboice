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

class BasicObject
  def self.subclasses
    @subclasses ||= []
  end

  def self.inherited a_subclass
    subclasses << a_subclass
    super
  end

  def self.all_leaf_subclasses
    all_subclasses.select do |a_class|
      a_class.subclasses.empty?
    end
  end

  def self.all_subclasses

    # This is the equivalent of doing:
    # scan = subclasses.clone
    # subclasses.each do |a_subclass|
    #  scan << a_subclass.all_subclasses
    # end
    # scan.flatten
    # But faster...

    scan = subclasses.clone
    index = 0
    while (index < scan.size) do
      scan << scan[index].subclasses
      scan.flatten!
      index += 1
    end
    scan
  end
end
