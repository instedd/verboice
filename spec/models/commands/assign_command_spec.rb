require 'spec_helper'

describe Commands::AssignCommand do
  before(:each) do
    @session = Session.new :pbx => mock('pbx')
  end

  it "assigns" do
    cmd = Commands::AssignCommand.new :name => 'foo', :expr => '1 + 2'
    cmd.run @session

    @session['foo'].should == 3
  end
end
