module Parsers
  module UserFlowNode
    class Recording < Message

      attr_reader :name

      def self.can_handle? params
        params['type'] == 'recording'
      end

      def initialize project, parent_step, action, params
        @project   = project
        @parent    = parent_step
        @name      = params['name']
        @file_name = (RecordingManager.new project).recording_path_for(parent_step.id, action)
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