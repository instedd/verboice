require 'spec_helper'

describe UserCommand do

  it "should list all available user commands" do
    UserCommand.all.size.should eq(1)
    UserCommand.all.should include(Menu)
  end
end