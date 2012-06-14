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
    class Recording < Message

      attr_reader :name

      def self.can_handle? params
        params['type'] == 'recording'
      end

      def initialize call_flow, parent_step, action, params
        @call_flow = call_flow
        @parent = parent_step
        @name = params['name']
        @file_name = (RecordingManager.new call_flow).recording_path_for(parent_step.id, action)
      end

      def equivalent_flow
        Commands::PlayFileCommand.new @file_name if File.exists?(@file_name)
      end

      def capture_flow
        { play_file: @file_name }
      end
    end
  end
end
