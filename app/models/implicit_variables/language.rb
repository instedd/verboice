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

module ImplicitVariables
  class Language < ImplicitVariable

    def value(use_default = true)
      persisted_variable = @contact.persisted_variables.find_by_implicit_key(self.class.key)
      if persisted_variable
        persisted_variable.typecasted_value
      elsif use_default
        @contact.project.default_language
      else
        nil
      end
    end

    def self.key
      'language'
    end

  end
end
