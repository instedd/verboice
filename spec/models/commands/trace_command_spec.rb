require 'spec_helper'

module Commands
  describe TraceCommand do
    let(:session) do
      s = Session.new :pbx => mock('pbx')
      s.expects(:call_id).returns 333
      s
    end

    it "should create a trace entrance for a given command" do

      cmd = TraceCommand.new(:application_id => 1, :step_id => 20, :step_name => 'bar', :store => '"foo"')
      cmd.next = :next
      cmd.run(session).should == :next

      Trace.all.size.should eq(1)
      Trace.first.result.should eq('foo')
      Trace.first.application_id.should eq(1)
      Trace.first.step_id.to_i.should eq(20)
      Trace.first.call_id.should eq(333)
      Trace.first.step_name.should eq('bar')
    end
  end
end