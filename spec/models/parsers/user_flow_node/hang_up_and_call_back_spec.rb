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
    describe HangUpAndCallBack do

      let(:call_flow) { CallFlow.make }

      it "should compile to a verboice equivalent flow" do
        File.stub(:exists?).and_return{true}
        mark_as_failed = HangUpAndCallBack.new call_flow, 'id' => 1,
          'type' => 'hang_up_and_call_back',
          'name' => 'HangUpAndCallBack'

        mark_as_failed.equivalent_flow.first.should eq(
          Compiler.parse do |c|
            c.Label 1
            c.StartUserStep :hangup_and_callback, 1, 'HangUpAndCallBack'
            c.HangupAndCallback(dial_prefix: nil, when: 'immediately', delay: '1h')
          end.first
        )
      end
    end
  end
end
