module Parsers
  module UserFlowNode
    class Transfer < UserCommand
      attr_reader :id, :name, :application
      attr_accessor :next

      def initialize application, params
        @id = params['id']
        @name = params['name'] || ''
        @address = params['address']
        @channel = params['channel']
        @application = application
        @next = params['next']
        @root_index = params['root']
      end

      def is_root?
        @root_index.present?
      end

      def root_index
        @root_index
      end

      def equivalent_flow
        channel_name = @channel.present? ? "channel #{@channel}" : 'current channel'
        Compiler.parse do |compiler|
          compiler.Label @id
          compiler.Assign "current_step", @id
          compiler.Trace context_for %("Transfer to #{@address} in #{channel_name}.")
          compiler.Dial @address, {:channel => @channel}
          compiler.append @next.equivalent_flow if @next
        end
      end
    end
  end
end
