module Parsers
  module UserFlowNode
    class Menu < UserCommand

      attr_reader :id, :explanation_text, :options, :timeout, :invalid_text, :end_call_text

      def initialize params
        @id = params[:id]
        @explanation_text = params[:data][:explanation_text]
        @options = params[:data][:options] || []
        @is_root = params[:root] || false
        @timeout = params[:data][:timeout] || 0
        @invalid_text = params[:data][:invalid_text]
        @end_call_text = params[:data][:end_call_text]
      end

      def solve_links_with nodes
        @options.each do |an_option|
          possible_nodes = nodes.select do |a_node|
            a_node.id == an_option[:next]
          end
          if possible_nodes.size == 1
            an_option[:next] = possible_nodes.first
          else
            if possible_nodes.size == 0
              raise "There is no command with id #{an_option[:next]}"
            else
              raise "There are multiple commands with id #{an_option[:next]}: #{possible_nodes.inspect}."
            end
          end
        end
      end

      def is_root?
        @is_root
      end

      def equivalent_flow
         @equivalent_flow ||= build_equivalent_flow
      end

      def build_equivalent_flow
        @equivalent_flow = []
        @equivalent_flow << {say: @explanation_text}
        if_conditions = []
        last_capture_hash = {}
        @options.each do |an_option|
          last_capture_hash = {
            capture: {
              timeout: 0,
              say: "#{an_option[:description]}"
            }
          }
          @equivalent_flow << last_capture_hash
          if_conditions << {
            :if => {
              :condition => "digits == #{an_option[:number]}",
              :then => an_option[:next].equivalent_flow
            }
          }
        end
        unless last_capture_hash.empty?
          last_capture_hash[:capture][:timeout] = @timeout
          last_if_condition = if_conditions.pop
          last_if_condition[:if][:else] = [
            {say: "invalid key pressed"},
            :hangout
          ]
          if_conditions.reverse.each do |an_if_condition_hash|
            an_if_condition_hash[:if][:else] = last_if_condition
            last_if_condition = an_if_condition_hash
          end
          @equivalent_flow << last_if_condition
        end
                # 
                # [
                #   {say: 'foobar'},
                #   {
                #     capture: {
                #       timeout: 0,
                #       say: 'foo'
                #     }
                #   },
                #   {
                #     capture: {
                #       timeout: 20,
                #       say: 'bar'
                #     }
                #   },
                #   {
                #     :if => {
                #       :condition => "digits == 10",
                #       :then => [{say: 'asdf'}],
                #       :else => [
                #         {
                #           :if => {
                #             :condition => "digits == 14",
                #             :then => [{say: 'qwer'}],
                #             :else => [
                #               {
                #                 :if => {
                #                   :condition => "digits == 5",
                #                   :then => [{say: 'qwer'}],
                #                   :else => [
                #                     {say: "invalid key pressed"},
                #                     :hangout
                #                   ]
                #                 }
                #               }
                #             ]
                #           }
                #         }
                #       ]
                #     }
                #   }
                # ]
        @equivalent_flow << {say: @end_call_text} if @end_call_text
        @equivalent_flow
      end
    end
  end
end