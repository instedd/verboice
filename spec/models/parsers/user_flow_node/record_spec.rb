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
    describe Record do

      let(:call_flow) { double('call_flow', :id => 5) }

      it "should compile to an equivalent flow" do
        record = Record.new call_flow, 'id' => 1,
          'type' => 'record',
          'name' => 'Record Step',
          'explanation_message' => {
            "name" => "Explanation message",
            "type" => "text"
          },
          'confirmation_message' => {
            "name" => "Confirmation message",
            "type" => "text"
          },
          'timeout' => 7,
          'stop_key' => '#'

        record.equivalent_flow.first.should eq(
          Compiler.parse do
            Label 1
            Assign "current_step", 1
            AssignValue "current_step_name", "Record Step"
            Trace call_flow_id: 5, step_id: 1, step_name: 'Record Step', store: %("Record message. Download link: " + record_url(1))
            Say "Explanation message"
            Record 1, 'Record Step', {:stop_keys => '#', :timeout => 7}
            Say "Confirmation message"
          end.first
        )
      end

    end
  end
end
