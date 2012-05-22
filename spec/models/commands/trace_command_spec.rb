require 'spec_helper'

module Commands
  describe TraceCommand do
    let(:session) do
      s = Session.new :pbx => mock('pbx')
      s.expects(:call_id).returns 333
      s
    end

    it "should create a trace entrance for a given command" do
      cmd = TraceCommand.new(:call_flow_id => 1, :step_id => 20, :step_name => 'bar', :store => '"foo"')
      cmd.next = :next
      cmd.run(session).should == :next

      Trace.all.size.should eq(1)
      Trace.first.result.should eq('foo')
      Trace.first.call_flow_id.should eq(1)
      Trace.first.step_id.to_i.should eq(20)
      Trace.first.call_id.should eq(333)
      Trace.first.step_name.should eq('bar')
    end

    it "should take the step id from a given expression" do
      cmd = Compiler.make do |c|
        c.Assign "current_step", 3
        c.Trace :call_flow_id => 2, :step_id => 'current_step', :step_name => '', :store => '"zzz"'
      end
      cmd.run(session).run(session)

      Trace.all.size.should eq(1)
      Trace.first.result.should eq('zzz')
      Trace.first.call_flow_id.should eq(2)
      Trace.first.step_id.to_i.should eq(3)
      Trace.first.call_id.should eq(333)
      Trace.first.step_name.should eq('')
    end
  end
end