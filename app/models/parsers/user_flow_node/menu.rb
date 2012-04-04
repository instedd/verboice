module Parsers
  module UserFlowNode
    class Menu < UserCommand

      attr_reader :id, :explanation_text, :options, :timeout, :invalid_text, :end_call_text

      def initialize params
        @id = params['id']
        @explanation_text = params['explanation_text']
        @options_text = params['options_text']
        @options = params['options'] || []
        @is_root = params['root'] || false
        @timeout = params['timeout'] || 5
        @invalid_text = params['invalid_text']
        @end_call_text = params['end_call_text']
      end

      def solve_links_with nodes
        @options.each do |an_option|
          possible_nodes = nodes.select do |a_node|
            a_node.id == an_option['next']
          end
          if possible_nodes.size == 1
            an_option['next'] = possible_nodes.first
          else
            if possible_nodes.size == 0
              raise "There is no command with id #{an_option['next']}"
            else
              raise "There are multiple commands with id #{an_option['next']}: #{possible_nodes.inspect}."
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
        @equivalent_flow << {say: @explanation_text} if @explanation_text
        if_conditions = []
        @options.each do |an_option|
          if_conditions << {
            :if => {
              :condition => "digits == #{an_option['number']}",
              :then => an_option['next'].equivalent_flow
            }
          }
        end
        unless if_conditions.empty?
          @equivalent_flow << if @options_text
            {
              capture: {
                timeout: @timeout,
                say: @options_text
              }
            }
          else
            {
              capture: {
                timeout: @timeout
              }
            }
          end
          last_if_condition = if_conditions.pop
          last_if_condition[:if][:else] = if @invalid_text
            [
              {say: @invalid_text},
              :hangout
            ]
          else
            [
              :hangout
            ]
          end
          if_conditions.reverse.each do |an_if_condition_hash|
            an_if_condition_hash[:if][:else] = last_if_condition
            last_if_condition = an_if_condition_hash
          end
          @equivalent_flow << last_if_condition
        else
           @equivalent_flow << { say: @options_text } if @options_text
        end
        @equivalent_flow << {say: @end_call_text} if @end_call_text
        @equivalent_flow
      end
    end
  end
end