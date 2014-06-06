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
    class HangUp < UserCommand
      attr_reader :id, :name, :call_flow

      def self.can_handle? params
        params['type'] == 'hang_up'
      end

      def initialize call_flow, params
        @id = params['id']
        @name = params['name'] || ''
        @call_flow = call_flow
        @root_index = params['root']
      end

      def next
        # There is no next after hang up
        nil
      end

      def next= command
        # There is no next after hang up
      end

      def is_root?
        @root_index.present?
      end

      def root_index
        @root_index
      end

      def equivalent_flow
        Compiler.parse do |compiler|
          compiler.Label @id
          compiler.Trace context_for '"Verboice ended call."'
          compiler.End
        end
      end
    end
  end
end
