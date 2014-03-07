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
    class Language < UserCommand
      attr_reader :id, :name, :call_flow
      attr_accessor :next

      def initialize call_flow, params
        @id = params['id']
        @name = params['name'] || ''
        @resource = Resource.new params['resource']
        @languages = call_flow.project.languages.map { |lang| lang["language"] }
        @call_flow = call_flow
        @next = params['next']
        @root_index = params['root']
        @force_question = params['force_question']
      end

      def is_root?
        @root_index.present?
      end

      def root_index
        @root_index
      end

      def equivalent_flow
        var_name = ImplicitVariables::Language.key

        Compiler.parse do |compiler|
          compiler.Label @id
          compiler.StartUserStep :language, @id, @name
          unless @force_question
            compiler.If "typeof(var_#{var_name}) != 'undefined'" do |c|
              c.Trace context_for %("Language already set: '" + var_#{var_name} + "'")
              c.SetStepResult :already_set, "var_#{var_name}"
              c.Goto "end#{@id}"
            end
          end
          @languages.each_with_index do |language, i|
            compiler.Capture({finish_on_key: '', timeout: i+1 == @languages.size ? 10 : 1}.merge(@resource.capture_flow(language)))
            compiler.If "digits != null" do |c|
              c.Goto "set_language#{@id}"
            end
          end
          compiler.Label "set_language#{@id}"
          @languages.each_with_index do |language, i|
            compiler.If "digits == #{i+1}" do |c|
              c.Trace context_for %("User selected #{language} language.")
              c.SetStepResult :language, "var_#{var_name}"
              c.PersistVariable var_name, "'#{language}'"
            end
          end
          compiler.Label "end#{@id}"
          compiler.append @next.equivalent_flow if @next
        end
      end
    end
  end
end
