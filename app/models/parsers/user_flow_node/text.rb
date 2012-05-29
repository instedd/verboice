module Parsers
  module UserFlowNode
    class Text < Message

      attr_reader :name

      def self.can_handle? params
        params['type'] == 'text'
      end

      def initialize call_flow, parent_step, action, params
        @call_flow = call_flow
        @parent = parent_step
        @name = params['name']
      end

      def equivalent_flow
        Commands::SayCommand.new @name if @name.presence
      end

      def capture_flow
        { say: @name }
      end
    end
  end
end
