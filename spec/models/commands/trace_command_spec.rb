# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

require 'spec_helper'

module Commands
  describe TraceCommand do
    let(:session) do
      s = Session.new pbx: double('pbx'), call_log: CallLog.make
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