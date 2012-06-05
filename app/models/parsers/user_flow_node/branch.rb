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
          c.Assign "current_step", @id
          c.Assign "current_step_name", "'#{@name}'"
          @options.each_with_index do |an_option, index|
            retrieve_variables c, an_option['conditions']
            c.If(merge_conditions_from(an_option['conditions'])) do |c|
              c.Trace context_for "\"Branch number #{index + 1} selected: '#{an_option['description']}'\""
              c.append(an_option['next'].equivalent_flow) if an_option['next']
              c.Goto("end#{@id}")
            end
          end
          c.Trace context_for '"No branch was selected."'
          c.Label("end#{@id}")
          c.append(@next.equivalent_flow) if @next
        end
      end

      def merge_conditions_from conditions
        if conditions.nil? or conditions.empty?
          'true'
        else
          conditions.collect do |condition|
            "(#{first_comparison_term_from condition} #{condition['operator']} #{second_comparison_term_from condition})"
          end.join(' && ')
        end
      end

      def first_comparison_term_from condition
        condition['step'].presence ? "value_#{condition['step']}" : "var_#{condition['variable']}"
      end

      def second_comparison_term_from condition
        condition['rhs_variable'].presence ? "var_#{condition['rhs_variable']}" : "#{condition['value']}"
      end

      def retrieve_variables compiler, conditions
        return if conditions.nil?
        conditions.collect do |condition|
          ['variable', 'rhs_variable'].collect do |var_name|
            condition[var_name].presence
          end
        end.flatten.compact.uniq.each do |variable|
          compiler.RetrieveVariable variable
        end
      end
    end
  end
end
