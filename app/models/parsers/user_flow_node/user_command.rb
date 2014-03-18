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

class Parsers::UserFlowNode::UserCommand

  def self.can_handle? params
    params['type'] == name[name.rindex('::') + 2 ... name.size].underscore
  end

  def self.for call_flow, params
    (SuitableClassFinder.find_direct_subclass_of self, suitable_for: params).new call_flow, params
  end

  def equivalent_flow
    subclass_responsibility
  end

  def is_root?
    subclass_responsibility
  end

  def root_index
    subclass_responsibility
  end

  def solve_links_with
    subclass_responsibility
  end

  def id
    subclass_responsibility
  end

  def call_flow
    subclass_responsibility
  end

  def name
    subclass_responsibility
  end

  def next
    subclass_responsibility
  end

  def next= a_node
    subclass_responsibility
  end

  def needs_call_to_be_answered?
    true
  end

  def context_for message
    {
      call_flow_id: call_flow.id,
      step_id: id,
      step_name: name,
      store: message
    }
  end

  def solve_links_with nodes
    self.next = node_linked_by self.next, nodes
  end

  def node_linked_by id, nodes
    if id && !id.is_a?(Parsers::UserFlowNode::UserCommand)
      possible_nodes = nodes.select do |a_node|
        a_node.id == id
      end
      if possible_nodes.size == 1
        possible_nodes.first
      else
        if possible_nodes.size == 0
          raise "There is no command with id #{id}"
        else
          raise "There are multiple commands with id #{id}: #{possible_nodes.inspect}."
        end
      end
    else
      id
    end
  end
end
