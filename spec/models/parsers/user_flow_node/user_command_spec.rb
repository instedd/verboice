require 'spec_helper'

module Parsers
  module UserFlowNode
    describe UserCommand do

      it "should list all available user commands" do
        UserCommand.subclasses.size.should eq(2)
        UserCommand.subclasses.should include(Menu)
        UserCommand.subclasses.should include(Play)
      end

      it "should deliver the right subclass to parse a given input" do
        (UserCommand.for self, 'id' => 27, 'type' => 'menu',
          'explanation_message' => {},
          'options_message' => {},
          'end_call_message' => {},
          'invalid_message' => {}).class.should eq(Menu)

        (UserCommand.for self, 'id' => 27, 'type' => 'play', 'message' => {}).class.should eq(Play)
      end
    end
  end
end