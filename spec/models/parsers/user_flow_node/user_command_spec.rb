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

      it "should list all available user commands" do
        UserCommand.subclasses.size.should eq(10)
        UserCommand.subclasses.should include(Menu)
        UserCommand.subclasses.should include(Play)
        UserCommand.subclasses.should include(Capture)
        UserCommand.subclasses.should include(Goto)
        UserCommand.subclasses.should include(Transfer)
        UserCommand.subclasses.should include(Branch)
        UserCommand.subclasses.should include(HangUp)
        UserCommand.subclasses.should include(Record)
        UserCommand.subclasses.should include(External)
        UserCommand.subclasses.should include(Language)
      end

      it "should deliver the right subclass to parse a given input" do
        (UserCommand.for self, 'id' => 27, 'type' => 'menu').class.should eq(Menu)

        (UserCommand.for self, 'id' => 27, 'type' => 'play').class.should eq(Play)

        (UserCommand.for self, 'id' => 27, 'type' => 'capture').class.should eq(Capture)

        (UserCommand.for self, 'id' => 27, 'type' => 'goto').class.should eq(Goto)

        (UserCommand.for self, 'id' => 27, 'type' => 'transfer').class.should eq(Transfer)

        (UserCommand.for self, 'id' => 27, 'type' => 'branch').class.should eq(Branch)

        (UserCommand.for self, 'id' => 27, 'type' => 'hang_up').class.should eq(HangUp)

        (UserCommand.for self, 'id' => 27, 'type' => 'record').class.should eq(Record)

        (UserCommand.for self, 'id' => 27, 'type' => 'external').class.should eq(External)
      end
    end
  end
end