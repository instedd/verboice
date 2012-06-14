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

module Freeswitch
  class Client < Librevox::Listener::Inbound
    event :background_job do |event|
      if event.body.match /^-ERR (.*)/
        error_message = $1
        args = CGI.unescape event.content[:job_command_arg]
        if args.match /verboice_call_log_id=(\d+)/
          call_log = CallLog.find($1) or return
          call_log.error "Failed to establish the communication: #{error_message}"
          call_log.finish :failed
        end
      end
    end
  end
end
