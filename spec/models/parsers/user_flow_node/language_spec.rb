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

require 'spec_helper'

module Parsers
  module UserFlowNode
    describe Language do

      let(:project) { Project.make :languages => [{'language' => 'en'}, {'language' => 'es'}] }
      let(:call_flow) { CallFlow.make :project => project }

      it "should compile to a verboice equivalent flow" do
        language = Language.new call_flow, 'id' => 1,
          'type' => 'language',
          'name' => 'Detect Language',
          'resource' => {'guid' => '12349'}

        language.equivalent_flow.first.should eq(
          Compiler.parse do |c|
            c.Label 1
            c.StartUserStep :language, 1, "Detect Language"
            c.If "typeof(var_language) != 'undefined'" do |c|
              c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Detect Language', store: %("Language already set: '" + var_language + "'")
              c.SetStepResult :already_set, "var_language"
              c.Goto "end1"
            end
            c.Capture({finish_on_key: '', timeout: 1, resource: '12349', language: 'en'})
            c.If "digits != null" do |c|
              c.Goto "set_language1"
            end
            c.Capture({finish_on_key: '', timeout: 10, resource: '12349', language: 'es'})
            c.If "digits != null" do |c|
              c.Goto "set_language1"
            end
            c.Label "set_language1"
            c.If "digits == 1" do |c|
              c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Detect Language', store: '"User selected en language."'
              c.SetStepResult :language, "var_language"
              c.PersistVariable 'language', "'en'"
            end
            c.If "digits == 2" do |c|
              c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Detect Language', store: '"User selected es language."'
              c.SetStepResult :language, "var_language"
              c.PersistVariable 'language', "'es'"
            end
            c.Label "end1"
          end.first
        )
      end

    end
  end
end
