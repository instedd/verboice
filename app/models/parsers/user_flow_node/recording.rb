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
