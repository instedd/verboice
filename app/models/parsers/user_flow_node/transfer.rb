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

module Parsers
  module UserFlowNode
    class Transfer < UserCommand
      attr_reader :id, :name, :call_flow
      attr_accessor :next

      def initialize call_flow, params
        @id = params['id']
        @name = params['name'] || ''
        @address = params['address']
        @channel = params['channel']
        @call_flow = call_flow
        @next = params['next']
        @root_index = params['root']
        @successful_after = params['successful_after']
      end

      def is_root?
        @root_index.present?
      end

      def root_index
        @root_index
      end

      def equivalent_flow
        channel_name = @channel.present? ? "channel #{@channel}" : 'current channel'
        Compiler.parse do |c|
          c.Label @id
          c.StartUserStep :transfer, @id, @name
          c.Trace context_for %("Transfer to #{@address} in #{channel_name}.")
          c.Dial @address, {:channel => @channel, :successful_after => @successful_after}
          c.SetStepResult [:eval, "dial_status"]
          c.append @next.equivalent_flow if @next
        end
      end
    end
  end
end
