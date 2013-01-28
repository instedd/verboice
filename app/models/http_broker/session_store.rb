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

module HttpBroker

  class SessionStore
    include Singleton

    Timeout = 5 * 60

    def initialize
      @sessions = {}
      @timers = {}
    end

    def session_for(key)
      renew_em_timer(key)
      @sessions[key] ||= HttpBroker::Session.new
    end

    private

    def renew_em_timer(key)
      current_timer = @timers[key]
      EM.cancel_timer(current_timer) if current_timer

      @timers[key] = EM.add_timer Timeout do
        @timers.delete key
        fiber = @sessions.delete(key).try(:get_fiber)
        fiber.resume(Exception.new("Session timeout")) if fiber
      end
    end

  end

  class Session

    def initialize
      @session = {}
      @fiber = nil
    end

    def store_fiber(fiber)
      @fiber = fiber
    end

    def get_fiber
      @fiber
    end

    def end!
      @fiber = nil
    end

    def store(key, value)
      @session[key] = value
    end

    def get(key)
      @session[key]
    end

  end

end