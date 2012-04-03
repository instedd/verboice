require 'spec_helper'

module Parsers
  module UserFlowNode
    describe UserCommand do

      it "should list all available user commands" do
        UserCommand.subclasses.size.should eq(1)
        UserCommand.subclasses.should include(Menu)
      end
  
      it "should deliver the right subclass to parse a given input" do
        (UserCommand.for id: 27, type: 'menu', data: {}).class.should eq(Menu)
      end
      
    end
  end
end