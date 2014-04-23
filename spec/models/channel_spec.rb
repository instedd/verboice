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

describe Channel do
  # Because we have after_commit callbacks...
  self.use_transactional_fixtures = false

  before(:each) do
    Timecop.freeze(Time.parse("2012-01-01T12:00:00Z"))
  end

  after(:each) do
    BrokerClient.stub(:destroy_channel)
    [Account, Channel.all_leaf_subclasses, CallLog, Schedule, QueuedCall].flatten.each &:destroy_all
    Timecop.return
  end

  Channel.all_leaf_subclasses.each do |a_channel|

    context "validations" do
      before(:each) { a_channel.make }

      it { should belong_to(:account) }
      it { should belong_to(:call_flow) }

      it { should validate_presence_of(:account) }
      it { should validate_presence_of(:name) }
      it { should validate_uniqueness_of(:name).scoped_to(:account_id) }
    end

    context "call" do
      let (:channel) { a_channel.make }
      let (:queued_call) { channel.reload.queued_calls.first }

      it "call ok" do
        BrokerClient.should_receive(:notify_call_queued).with(channel.id)

        call_log = channel.call 'foo'
        call_log.state.should == :queued
        call_log.address.should == 'foo'

        queued_calls = channel.queued_calls
        queued_calls.length.should == 1
        queued_calls[0].address.should == 'foo'
        queued_calls[0].call_log_id.should == call_log.id
      end

      it "call raises" do
        BrokerClient.should_receive(:notify_call_queued).with(channel.id).and_raise("Oh no!")

        call_log = channel.call 'foo'
        call_log.state.should == :queued
      end

      it "call and set direction outgoing" do
        BrokerClient.should_receive(:notify_call_queued)

        call_log = channel.call 'foo'
        call_log.direction.should == :outgoing
      end

      it "call with custom callback url" do
        BrokerClient.should_receive(:notify_call_queued)

        channel.call 'foo', :callback_url => 'bar'
        queued_call.callback_url.should == 'bar'
        queued_call.call_flow.should be_nil
      end

      it "call with custom flow" do
        BrokerClient.should_receive(:notify_call_queued)
        channel.call 'foo', :flow => Compiler.make { Answer(); Hangup() }
        queued_call.flow.should == Compiler.make { Answer(); Hangup() }.to_a
      end

      it "call with custom status callback url" do
        BrokerClient.should_receive(:notify_call_queued)

        channel.call 'foo', :status_callback_url => 'bar'
        queued_call.status_callback_url.should == 'bar'
      end

      it "notify with time when scheduling delayed call" do
        time = Time.now.utc + 1.hour
        BrokerClient.should_receive(:notify_call_queued).with(channel.id, time)
        channel.call 'foo', :not_before => time
      end

      it "notify with time when scheduling delayed call with time as string" do
        time = Time.now.utc + 1.hour
        BrokerClient.should_receive(:notify_call_queued).with(channel.id, time).once
        channel.call 'foo', :not_before => time.to_s
      end

      it "obey queue lower time bound" do
        schedule = channel.project.schedules.make :time_from => '10:00', :time_to => '12:00'
        BrokerClient.should_receive(:notify_call_queued)
        channel.call 'foo', :not_before => '2012-12-20T08:00:00Z', :schedule_id => schedule.id
        queued_call.not_before.should == '2012-12-20T10:00:00Z'
      end

      it "obey queue upper time bound" do
        schedule = channel.project.schedules.make :time_from => '10:00', :time_to => '12:00'
        BrokerClient.should_receive(:notify_call_queued)
        channel.call 'foo', :not_before => '2012-12-20T13:00:00', :schedule_id => schedule.id
        queued_call.not_before.should == '2012-12-21T10:00:00'
      end

      it "uses selected time zone for 'not before' date" do
        BrokerClient.should_receive(:notify_call_queued)
        channel.call_flow.project.update_attribute :time_zone, 'Buenos Aires'
        channel.reload.call 'foo', :not_before => '2012-12-20T10:00:00', :time_zone => 'Paris'
        queued_call.not_before.should eq(Time.parse('2012-12-20T09:00:00 UTC'))
      end

      it "uses project's time zone for 'not before' date" do
        BrokerClient.should_receive(:notify_call_queued)
        channel.call_flow.project.update_attribute :time_zone, 'Buenos Aires'
        channel.reload.call 'foo', :not_before => '2012-12-20T10:00:00'
        queued_call.not_before.should eq(Time.parse('2012-12-20T13:00:00 UTC'))
      end

      it "uses project's time zone when not before is nil" do
        BrokerClient.should_receive(:notify_call_queued)
        channel.call_flow.project.update_attribute :time_zone, 'Buenos Aires'
        channel.reload.call 'foo'
        queued_call.time_zone.should eq('America/Argentina/Buenos_Aires')
      end

      it "calls with variables" do
        BrokerClient.should_receive(:notify_call_queued)
        call_log = channel.call 'foo', vars: {'bar' => '1', 'baz' => 'eee'}
        queued_call.variables.should eq({'bar' => 1, 'baz' => 'eee'})
      end
    end

    it "call create_channel on broker client when create" do
      channel = a_channel.make_unsaved
      BrokerClient.should_receive(:create_channel).with do |channel_id|
        channel_id == channel.id
      end
      channel.save!
    end

    if a_channel.is_a?(Channels::Sip)
      it "register? and_return true" do
        channel = a_channel.new :config => { 'register' => 'true' }
        channel.register?.should_not be_nil
      end

      it "register? and_return false" do
        channel = a_channel.new
        channel.register?.should be_false
      end
    end
  end

  it "should assign a guid" do
    channel = Channels::Voxeo.make_unsaved
    channel.guid.should be_nil
    channel.save!
    channel.guid.should_not be_nil
  end
end
