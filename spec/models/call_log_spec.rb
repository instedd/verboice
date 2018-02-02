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
  it { is_expected.to belong_to(:account) }
  it { is_expected.to belong_to(:project) }
  it { is_expected.to belong_to(:channel) }

  it { is_expected.to validate_presence_of(:account) }
  it { is_expected.to validate_presence_of(:project) }
  it { is_expected.to validate_presence_of(:channel) }

  it "call log structured details" do
    call = CallLog.make
    Timecop.freeze(Time.local(2012, 1, 1, 0, 0, 13))
    CallLogEntry.make description: 'Answer', call: call, severity: :info
    Timecop.freeze(Time.local(2012, 1, 1, 0, 12, 25))
    CallLogEntry.make description: 'Callback http://localhost:4567 with CallSid=b1cc8e26-21b3-1b16-d97d-bf18033e314d&Digits=', severity: :trace, call: call
    Timecop.freeze(Time.local(2012, 1, 1, 1, 23, 48))
    CallLogEntry.make description: 'Callback returned: http://localhost:4567/guess.mp3', severity: :trace, call: call

    details = call.structured_details
    expect(details.length).to eq(3)
    assert_equal({:severity => :info, :time => Time.local(2012, 1, 1, 0, 0, 13), :text => 'Answer'}, details[0])
    assert_equal({:severity => :trace, :time => Time.local(2012, 1, 1, 0, 12, 25), :text => 'Callback http://localhost:4567 with CallSid=b1cc8e26-21b3-1b16-d97d-bf18033e314d&Digits='}, details[1])
    assert_equal({:severity => :trace, :time => Time.local(2012, 1, 1, 1, 23, 48), :text => 'Callback returned: http://localhost:4567/guess.mp3'}, details[2])
    Timecop.return
  end

  it "create for project assigns account" do
    channel = Channel.all_leaf_subclasses.sample.make
    call_flow = channel.call_flow
    call_log = call_flow.call_logs.create! :channel => channel
    expect(call_log.account_id).to eq(call_flow.account.id)
  end

  it "save started at when starting an outgoing call" do
    call_log = CallLog.make
    expect(call_log.started_at).to eq(nil)

    time = Time.now
    allow(Time).to receive_messages(:now => time)

    call_log.start_outgoing '1234'
    expect(call_log.started_at).to eq(time)
    assert_match /Calling 1234/, call_log.entries.first.description
    expect(call_log.state).to eq(:active)
    expect(call_log.address).to eq('1234')
  end

  context "finishing call" do

    let(:call_flow) do
      call_flow = CallFlow.make :fusion_table_name => "my_table"
    end

    let(:call_log) do
      call_log = CallLog.make :call_flow => call_flow
      call_log.call_flow.store_in_fusion_tables = true
      call_log
    end

    before(:each) do
      expect_any_instance_of(CallFlow).to receive(:push_results).with(call_log)
    end

    after(:each) do
      allow_any_instance_of(CallFlow).to receive(:push_results).and_call_original
    end

    it "should push data on success" do
      call_log.start
      call_log.finish_successfully
      expect(call_log.state).to eq(:completed)
    end

    it "should push data on error" do
      call_log.start
      call_log.finish_with_error("Error")
      expect(call_log.state).to eq(:failed)
    end

  end

  it 'should return last entry' do
    call = CallLog.make
    expect(call.last_entry).to be_nil
    entry1 = CallLogEntry.make :call => call
    expect(call.last_entry).to eq(entry1)
    entry2 = CallLogEntry.make :call => call
    expect(call.last_entry).to eq(entry2)
  end
end
