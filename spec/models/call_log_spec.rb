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

describe CallLog do
  it { should belong_to(:account) }
  it { should belong_to(:project) }
  it { should belong_to(:channel) }

  it { should validate_presence_of(:account) }
  it { should validate_presence_of(:project) }
  it { should validate_presence_of(:channel) }

  it "call log structured details" do
    call = CallLog.make
    Timecop.freeze(Time.local(2012, 1, 1, 0, 0, 13))
    CallLogEntry.make description: 'Answer', call: call, severity: :info
    Timecop.freeze(Time.local(2012, 1, 1, 0, 12, 25))
    CallLogEntry.make description: 'Callback http://localhost:4567 with CallSid=b1cc8e26-21b3-1b16-d97d-bf18033e314d&Digits=', severity: :trace, call: call
    Timecop.freeze(Time.local(2012, 1, 1, 1, 23, 48))
    CallLogEntry.make description: 'Callback returned: http://localhost:4567/guess.mp3', severity: :trace, call: call

    details = call.structured_details
    details.length.should == 3
    assert_equal({:severity => :info, :time => Time.local(2012, 1, 1, 0, 0, 13), :text => 'Answer'}, details[0])
    assert_equal({:severity => :trace, :time => Time.local(2012, 1, 1, 0, 12, 25), :text => 'Callback http://localhost:4567 with CallSid=b1cc8e26-21b3-1b16-d97d-bf18033e314d&Digits='}, details[1])
    assert_equal({:severity => :trace, :time => Time.local(2012, 1, 1, 1, 23, 48), :text => 'Callback returned: http://localhost:4567/guess.mp3'}, details[2])
    Timecop.return
  end

  it "create for project assigns account" do
    channel = Channel.make
    call_flow = channel.call_flow
    call_log = call_flow.call_logs.create! :channel => channel
    call_log.account_id.should == call_flow.account.id
  end

  it "save started at when starting an outgoing call" do
    call_log = CallLog.make
    call_log.started_at.should == nil

    time = Time.now
    Time.stub(:now => time)

    call_log.start_outgoing '1234'
    call_log.started_at.should == time
    assert_match /Calling 1234/, call_log.entries.first.description
    call_log.state.should == :active
    call_log.address.should == '1234'
  end

  context "finishing call" do

    let(:call_flow) do
      CallFlow.make
    end

    let(:call_log) do
      call_log = CallLog.make :call_flow => call_flow
    end

    before(:each) do
      call_flow.should_receive(:push_results).with(call_log)
      call_log.start
    end

    it "should push data on success" do
      call_log.finish_successfully
      call_log.state.should eq(:completed)
    end

    it "should push data on error" do
      call_log.finish_with_error("Error")
      call_log.state.should eq(:failed)
    end

  end
end
