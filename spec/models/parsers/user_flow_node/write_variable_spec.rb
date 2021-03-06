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
    describe WriteVariable do

      let(:call_flow) { CallFlow.make }

      it "should compile to a verboice equivalent flow" do
        write_var = WriteVariable.new call_flow, 'id' => 1,
          'type' => 'write_variable',
          'name' => 'WriteVariable',
          'variable' => 'foo',
          'value' => 42

        expect(write_var.equivalent_flow.first).to eq(
          Compiler.parse do |c|
            c.Label 1
            c.StartUserStep :write_variable, 1, 'WriteVariable'
            c.PersistVariable 'foo', "'42'"
            c.SetStepResult "foo = '42'"
            c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'WriteVariable', store: '"Variable written."'
          end.first
        )
      end

      it "should compile without PersisVariable if variable is empty" do
        write_var = WriteVariable.new call_flow, 'id' => 1,
          'type' => 'write_variable',
          'name' => 'WriteVariable',
          'variable' => '',
          'value' => 42

        expect(write_var.equivalent_flow.first).to eq(
          Compiler.parse do |c|
            c.Label 1
            c.StartUserStep :write_variable, 1, 'WriteVariable'
            c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'WriteVariable', store: '"Variable written."'
          end.first
        )
      end
    end
  end
end
