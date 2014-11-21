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
    describe Transfer do

      let(:call_flow) { double('call_flow', :id => 1) }

      it "should compile to an equivalent flow" do
        transfer = Transfer.new call_flow, 'id' => 1,
          'type' => 'transfer',
          'name' => 'Transfer',
          'channel' => 'foo',
          'address' => '1234-5678'

        transfer.equivalent_flow.first.should eq(
          Compiler.parse do
            Label 1
            StartUserStep :transfer, 1, "Transfer"
            Trace call_flow_id: 1, step_id: 1, step_name: 'Transfer', store: '"Transfer to 1234-5678 in channel foo."'
            Dial '1234-5678', {:channel => 'foo'}
            SetStepResult [:eval, "dial_status"]
          end.first
        )
      end

      it "should compile with current channel" do
        transfer = Transfer.new call_flow, 'id' => 2,
          'type' => 'transfer',
          'name' => 'Transfer',
          'address' => '1234-5678'

        transfer.equivalent_flow.first.should eq(
          Compiler.parse do
            Label 2
            StartUserStep :transfer, 2, "Transfer"
            Trace call_flow_id: 1, step_id: 2, step_name: 'Transfer', store: '"Transfer to 1234-5678 in current channel."'
            Dial '1234-5678', {:channel => nil}
            SetStepResult [:eval, "dial_status"]
          end.first
        )
      end

    end
  end
end
