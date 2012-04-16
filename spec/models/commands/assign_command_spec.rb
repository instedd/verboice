require 'spec_helper'

describe Commands::AssignCommand do
  before(:each) do
    @session = Session.new :pbx => mock('pbx')
  end

  it "assigns" do
    cmd = Commands::AssignCommand.new 'foo', '1 + 2'
    cmd.next = :next
    cmd.run(@session).should == :next

    @session['foo'].should == 3
  end
end
