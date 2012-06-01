require 'spec_helper'

module Parsers
  module UserFlowNode
    describe UserCommand do

      it "should list all available user commands" do
        UserCommand.subclasses.size.should eq(9)
        UserCommand.subclasses.should include(Menu)
        UserCommand.subclasses.should include(Play)
        UserCommand.subclasses.should include(Capture)
        UserCommand.subclasses.should include(Goto)
        UserCommand.subclasses.should include(Transfer)
        UserCommand.subclasses.should include(Branch)
        UserCommand.subclasses.should include(HangUp)
        UserCommand.subclasses.should include(Record)
        UserCommand.subclasses.should include(External)
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