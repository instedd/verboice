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

# Be sure to restart your server when you modify this file.

Verboice::Application.config.session_store :cookie_store, :key => '_verboice_session'

# Use the database for sessions instead of the cookie-based default,
# which shouldn't be used to store highly confidential information
# (create the session table with "rails generate session_migration")
# Verboice::Application.config.session_store :active_record_store


#
# Mark cookies as secure when created in a secure (SSL) connection.
#
# Needed to monkeypatch in order to decide this for every cookie being
# created, as we want to support both secure and non secure sessions.
#
module ActionDispatch
  class Cookies::CookieJar

    alias_method :old_set_cookie_value, :[]=

    def []=(key, options)
      # @secure is initialized when the cookiejar is created, and is
      # true iff the request is made over an ssl connection.
      #
      # see CookieJar in ActionPack gem.
      options[:secure] = @secure
      old_set_cookie_value(key, options)
    end

  end
end
