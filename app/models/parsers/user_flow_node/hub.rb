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
    class Hub < UserCommand
      attr_reader :id, :name, :call_flow
      attr_accessor :action_name
      attr_accessor :action_path
      attr_accessor :bindings
      attr_accessor :next

      def self.can_handle? params
        params['type'] == 'hub'
      end

      def initialize call_flow, params
        @id = params['id']
        @name = params['name'] || ''
        @bindings = params['bindings']
        @call_flow = call_flow
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
        Compiler.parse do |compiler|
          compiler.Label @id
          compiler.Trace context_for '"Hub."'
          compiler.Js bindings_js()
          # compiler.Callback ...
          compiler.End
        end
      end

      def bindings_js
        str = ""
        str << "hub_payload = {};"
        @bindings.each do |binding|
          binding_js(binding, str, "hub_payload")
        end
        str
      end

      def binding_js(binding, str, prefix)
        name = binding['name']
        value = binding['value']
        new_prefix = "#{prefix}['#{name}']"

        if value
          input_setting = InputSetting.new(value)
          str << "#{new_prefix} = #{input_setting.expression};"
        else
          sub_bindings = binding['bindings']
          str << "#{new_prefix} = {};"
          sub_bindings.each do |sub_binding|
            binding_js(sub_binding, str, new_prefix)
          end
        end
      end
    end
  end
end
