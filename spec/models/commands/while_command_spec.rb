require 'spec_helper'

module Commands
  describe WhileCommand do
    before(:each) do
      @session = Session.new :pbx => mock('pbx')
    end

    it "while when true" do
      @session['i'] = 0

      cmd = WhileCommand.new 'i == 0', AssignCommand.new(:i, 'i + 1')
      result = cmd.run(@session)
      result.should be_instance_of(Commands::AssignCommand)
      result.next.should be(cmd)
    end

    it "while when false" do
      @session['i'] = 0

      cmd = WhileCommand.new 'i != 0', AssignCommand.new(:i, 'i + 1')
      cmd.next = :next
      cmd.run(@session).should == :next
    end
  end
end