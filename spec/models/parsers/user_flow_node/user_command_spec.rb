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
    describe UserCommand do
      it "should deliver the right subclass to parse a given input" do
        expect((UserCommand.for self, 'id' => 27, 'type' => 'menu').class).to eq(Menu)
        expect((UserCommand.for self, 'id' => 27, 'type' => 'play').class).to eq(Play)
        expect((UserCommand.for self, 'id' => 27, 'type' => 'capture').class).to eq(Capture)
        expect((UserCommand.for self, 'id' => 27, 'type' => 'goto').class).to eq(Goto)
        expect((UserCommand.for self, 'id' => 27, 'type' => 'transfer').class).to eq(Transfer)
        expect((UserCommand.for self, 'id' => 27, 'type' => 'branch').class).to eq(Branch)
        expect((UserCommand.for self, 'id' => 27, 'type' => 'hang_up').class).to eq(HangUp)
        expect((UserCommand.for self, 'id' => 27, 'type' => 'record').class).to eq(Record)
        expect((UserCommand.for self, 'id' => 27, 'type' => 'external').class).to eq(External)
        expect((UserCommand.for self, 'id' => 27, 'type' => 'mark_as_failed').class).to eq(MarkAsFailed)
        expect((UserCommand.for self, 'id' => 27, 'type' => 'hang_up_and_call_back').class).to eq(HangUpAndCallBack)
        expect((UserCommand.for self, 'id' => 27, 'type' => 'mark_as_successful').class).to eq(MarkAsSuccessful)
      end
    end
  end
end