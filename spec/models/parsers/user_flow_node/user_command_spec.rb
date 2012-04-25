require 'spec_helper'

module Parsers
  module UserFlowNode
    describe UserCommand do

      it "should list all available user commands" do
        UserCommand.subclasses.size.should eq(6)
        UserCommand.subclasses.should include(Menu)
        UserCommand.subclasses.should include(Play)
        UserCommand.subclasses.should include(Capture)
        UserCommand.subclasses.should include(Goto)
        UserCommand.subclasses.should include(Transfer)
        UserCommand.subclasses.should include(Branch)
      end

      it "should deliver the right subclass to parse a given input" do
        (UserCommand.for self, 'id' => 27, 'type' => 'menu',
          'explanation_message' => {},
          'options_message' => {},
          'end_call_message' => {},
          'invalid_message' => {}).class.should eq(Menu)

        (UserCommand.for self, 'id' => 27, 'type' => 'play', 'message' => {}).class.should eq(Play)
        
        (UserCommand.for self, 'id' => 27, 'type' => 'capture',
          'instructions_message' => {},
          'end_call_message' => {},
          'invalid_message' => {}).class.should eq(Capture)
          
        (UserCommand.for self, 'id' => 27, 'type' => 'goto', 'message' => {}).class.should eq(Goto)
      
        (UserCommand.for self, 'id' => 27, 'type' => 'transfer', 'message' => {}).class.should eq(Transfer)
      
        (UserCommand.for self, 'id' => 27, 'type' => 'branch', 'message' => {}).class.should eq(Branch)
      end
    end
  end
end