require 'spec_helper'

module Commands
  describe WhileCommand do
    before(:each) do
      @session = Session.new :pbx => mock('pbx')
    end

    it "while when true" do
      @session['i'] = 0

      do_commands = {:assign => {:name => :i, :expr => 'i + 1'}}
      cmd = WhileCommand.new :condition => 'i == 0', :do => do_commands

      @session.should_receive(:push_commands).with([do_commands, cmd])

      cmd.run @session
    end

    it "while when false" do
      @session['i'] = 0

      do_commands = {:assign => {:name => :i, :expr => 'i + 1'}}
      cmd = WhileCommand.new :condition => 'i != 0', :do => do_commands

      @session.should_receive(:push_commands).never

      cmd.run @session
    end
  end
end