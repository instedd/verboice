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
    class Capture < UserCommand
      attr_reader :id, :name, :call_flow
      attr_accessor :next

      def initialize call_flow, params
        @id = params['id']
        @name = params['name'] || ''
        @root_index = params['root']
        @instructions_resource = Resource.new params['instructions_resource']
        @valid_values = params['valid_values']
        @finish_on_key = params['finish_on_key'] || self.class.default_finish_key
        @min_input_length = params['min_input_length'].try(:to_i) || self.class.default_minimum_input_lenght
        @max_input_length = params['max_input_length'].try(:to_i) || self.class.default_maximum_input_lenght
        @timeout = params['timeout'].try(:to_i) || self.class.default_time_out_in_seconds
        @number_of_attempts = params['number_of_attempts'] || self.class.default_number_of_attempts
        @invalid_resource = Resource.new params['invalid_resource']
        @call_flow = call_flow
        @next = params['next']
        @persisted_variable_name = params['store']
        @default = params['default']
      end

      def solve_links_with nodes
        @default = node_linked_by @default, nodes
        super
      end

      def is_root?
        @root_index.present?
      end

      def root_index
        @root_index
      end

      def equivalent_flow
        Compiler.parse do |c|
          c.Label @id
          c.StartUserStep :input, @id, @name
          c.AssignValue "attempt_number#{@id}", 1
          c.While "attempt_number#{@id} <= #{@number_of_attempts}" do |c|
            c.Capture({
                min: @min_input_length,
                max: @max_input_length,
                finish_on_key: @finish_on_key,
                timeout: @timeout
              }.merge( @instructions_resource.capture_flow ))
            c.Assign "value_#{@id}", 'digits'
            c.PersistVariable @persisted_variable_name, "value_#{@id}" if @persisted_variable_name
            c.If valid_digits_condition do |c|
              c.SetStepResult :pressed, "digits"
              c.Goto "end#{@id}"
            end

            invalid_resource_block = lambda { |c|
              c.SetStepResult :invalid_key
              c.Trace context_for '"Invalid key pressed"'
              c.append @invalid_resource.equivalent_flow
            }

            if @min_input_length == 0
              c.Else &invalid_resource_block
            else
              unless @valid_values.blank?
                c.If "digits != null", &invalid_resource_block
              end
              c.Else do |c|
                c.SetStepResult :timeout
              end
            end
            c.Assign "attempt_number#{@id}", "attempt_number#{@id} + 1"
          end
          c.Trace context_for %("Missed input for #{@number_of_attempts} times.")
          c.append @default.equivalent_flow if @default
          c.Label "end#{@id}"
          c.append @next.equivalent_flow if @next
        end
      end

      def valid_digits_condition
        if @valid_values && !@valid_values.blank?
          conditions = @valid_values.split(/\s*[,;]\s*/).map do |clause|
            items = clause.split(/\s*-\s*/)
            if items.length == 1
              "(digits == #{items.first})"
            else
              "(digits >= #{items.first} && digits <= #{items.last})"
            end
          end
          conditions << '(digits == null)' if @min_input_length == 0
          conditions.join(' || ')
        else
          if @min_input_length == 0
            'true'
          else
            'digits != null'
          end
        end
      end

      def self.default_number_of_attempts
        Menu.default_number_of_attempts
      end

      def self.default_time_out_in_seconds
        Commands::CaptureCommand.default_time_out_in_seconds
      end

      def self.default_minimum_input_lenght
        Commands::CaptureCommand.default_minimum_input_lenght
      end

      def self.default_maximum_input_lenght
        Commands::CaptureCommand.default_maximum_input_lenght
      end

      def self.default_finish_key
        Commands::CaptureCommand.default_finish_key
      end
    end
  end
end
