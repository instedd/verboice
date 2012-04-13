module Parsers
  module UserFlowNode
    class Recording < Message

      attr_reader :name

      def self.can_handle? params
        params['type'] == 'record'
      end

      def initialize application, parent_step, action, params
        @application = application
        @parent = parent_step
        @name = params['name']
        @file_name = (RecordingManager.new application).get_recording_path_for(parent_step.id, action)
      end

      def equivalent_flow
        { play_file: @file_name }
      end

      def capture_flow
        { play_file: @file_name }
      end

    end
  end
end