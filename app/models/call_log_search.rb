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

module CallLogSearch
  extend ActiveSupport::Concern

  module ClassMethods
    def search(search, options = {})
      result = where '1 = 1'

      search = Search.new search

      if search.search
        result = result.where "call_logs.id = :search OR call_logs.address = :search OR call_logs.state = :search OR call_logs.direction = :search", :search => search.search
      end

      if search[:id]
        op, val = Search.get_op_and_val search[:id]
        result = result.where "call_logs.id #{op} ?", val.to_i
      end

      result = result.where "direction = ?", search[:direction] if search[:direction]
      result = result.where "state = ?", search[:state] if search[:state]

      [:address, :caller, :caller_id].each do |sym|
        result = result.where "address = ?", search[sym] if search[sym]
      end

      if search[:after]
        after = Time.smart_parse search[:after]
        result = result.where "started_at >= ?", after if after
      end
      if search[:before]
        before = Time.smart_parse search[:before]
        result = result.where "started_at <= ?", before if before
      end

      if search[:channel]
        if options[:account]
          channel = options[:account].channels.select(:id).find_by_name search[:channel]
          if channel
            result = result.where 'channel_id = ?', channel.id
          else
            result = result.where '1 = 2'
          end
        else
          result = result.joins(:channel).where 'channels.name = ?', search[:channel]
        end
      end
      if search[:project_id]
        result = result.where 'project_id = ?', search[:project_id]
      end
      if search[:project]
        if options[:account]
          app = options[:account].projects.select(:id).find_by_name search[:project]
          if app
            result = result.where 'project_id = ?', app.id
          else
            result = result.where '1 = 2'
          end
        else
          result = result.joins(:project).where 'projects.name = ?', search[:project]
        end
      end

      result
    end
  end
end
