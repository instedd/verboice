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
  class UserFlow

    attr_reader :call_flow

    def initialize call_flow, user_flow
      @user_flow = user_flow
      @roots = []
      @nodes = []
      @call_flow = call_flow
      build_nodes
    end

    def build_nodes
      @nodes = []

      @user_flow.each do | an_ui_command |
        @nodes << (Parsers::UserFlowNode::UserCommand.for call_flow, an_ui_command)
      end

      @nodes.each do | a_command_parser |
        a_command_parser.solve_links_with @nodes
      end

      @roots = @nodes.select(&:is_root?).sort { |a, b| a.root_index <=> b.root_index }
    end

    def equivalent_flow
      @equivalent_flow ||= build_equivalent_flow
    end

    def variables
      equivalent_flow
      @variables
    end

    def external_service_guids
      equivalent_flow
      @external_service_guids
    end

    def resource_guids
      equivalent_flow
      @resource_guids
    end

    def build_equivalent_flow
      answered = false

      Compiler.make do |compiler|
        @roots.each do |a_root_node|
          if a_root_node.needs_call_to_be_answered?
            compiler.Answer
            answered = false
          end
          compiler.append a_root_node.equivalent_flow
          compiler.End
        end
        @variables = compiler.variables.clone
        @external_service_guids = compiler.external_service_guids.clone
        @resource_guids = compiler.resource_guids.clone
      end
    end

    def step_names
      Hash[@nodes.reject do |node|
        node.is_a? UserFlowNode::Goto
      end.collect do |node|
        [node.id, node.name]
      end]
    end
  end
end
