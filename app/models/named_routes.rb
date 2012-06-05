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

class NamedRoutes
  include Singleton
  include Rails.application.routes.url_helpers

  DefaultUrlOptions = Rails.configuration.verboice_configuration[:default_url_options].to_hash.symbolize_keys!

  def self.default_url_options
    DefaultUrlOptions
  end

  def self.method_missing(method_sym, *arguments, &block)
    self.instance.send method_sym, *arguments, &block
  end
end