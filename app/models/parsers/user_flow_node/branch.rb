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
    class Branch < UserCommand
      attr_reader :id, :options, :name, :call_flow
      attr_accessor :next

      def initialize call_flow, params
        @id         = params['id']
        @name       = params['name'] || ''
        @options    = params['options'].deep_clone || []
        @root_index = params['root']
        @call_flow = call_flow
        @next = params['next']
      end

      def solve_links_with nodes
        @options.each do |an_option|
          an_option['next'] = node_linked_by an_option['next'], nodes
        end
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
          c.StartUserStep :branch, @id, @name
          @options.each_with_index do |an_option, index|
            c.If(merge_conditions_from(an_option['conditions'], c)) do |c|
              c.Trace context_for "\"Branch number #{index + 1} selected: '#{an_option['next'].name if an_option['next']}'\""
              c.SetStepResult :selected, "#{index + 1}"
              c.append(an_option['next'].equivalent_flow) if an_option['next']
              c.Goto("end#{@id}")
            end
          end
          c.Trace context_for '"No branch was selected."'
          c.SetStepResult :no_branch
          c.Label("end#{@id}")
          c.append(@next.equivalent_flow) if @next
        end
      end

      def merge_conditions_from conditions, compiler
        if conditions.nil? or conditions.empty?
          'true'
        else
          string_condition = conditions.map do |condition|
            if condition['operator'] == 'def' || condition['operator'] == 'undef'
              lhs = InputSetting.new(variable: condition['variable'], response: condition['response'], step: condition['step'])
              if condition['operator'] == 'def'
                "(typeof(#{lhs.expression()}) != 'undefined')"
              else
                "(typeof(#{lhs.expression()}) == 'undefined')"
              end
            else
              lhs = InputSetting.new(variable: condition['variable'], response: condition['response'], step: condition['step'])
              rhs = InputSetting.new(variable: condition['rhs_variable'], response: condition['rhs_response'], step: condition['rhs_step'], value: (condition['rhs_value'] || condition['value']))
              "(typeof(#{lhs.expression()}) != 'undefined' && typeof(#{rhs.expression()}) != 'undefined' && #{lhs.expression()} #{condition['operator']} #{rhs.expression()})"
            end
          end.join(' && ')
          string_condition
        end
      end
    end
  end
end
