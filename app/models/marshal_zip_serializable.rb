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

module MarshalZipSerializable
  extend ActiveSupport::Concern

  module ClassMethods
    def dump(x)
      data = nil

      # Marshal.dump fails with large flows when run inside a Fiber (Fiber stacks are 4k only)
      # Run the marshaling in a thread so we can use a full stack
      Thread.new { data = Zlib.deflate(Marshal.dump(x), 9) }.join
      data
    end

    def load(x)
      return nil if x.nil?
      data = nil

      # Marshal.load fails with large flows when run inside a Fiber (Fiber stacks are 4k only)
      # Run the unmarshaling in a thread so we can use a full stack
      Thread.new {
        begin
          data = Marshal.load(Zlib.inflate(x))
        rescue
          begin
            data = if x.start_with?("---")
              YAML.load(x)
            else
              JSON.parse(x).with_indifferent_access
            end
          rescue
            nil
          end
        end
      }.join
      data
    end
  end

end
