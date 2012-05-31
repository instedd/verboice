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

require 'fiber'

class Fiber
  class << self
    def yield_with_exception *args
      result = yield_without_exception *args
      raise result if result.is_an? Exception
      result
    end
    alias_method_chain :yield, :exception
  end

  def resume_with_exception(*args)
    if args.length == 1 && args[0].is_an?(Exception) && self == Fiber.current
      raise args[0]
    end
    resume_without_exception *args
  end
  alias_method_chain :resume, :exception
end
