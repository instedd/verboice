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

  let(:broker_client) { double('broker_client') }

  before(:each) do
    Timecop.freeze(Date.today)
    BrokerClient.stub(:new).and_return(broker_client)
  end

  after(:each) do
    broker_client.stub(:delete_channel)
    [Account, Channel.all_leaf_subclasses, CallLog, Schedule, QueuedCall].flatten.each &:destroy_all
    Timecop.return
  end

  Channel.all_leaf_subclasses.each do |a_subclass|

    context "validations" do
      before(:each) { a_subclass.make }

      it { should belong_to(:account) }
      it { should belong_to(:call_flow) }

      it { should validate_presence_of(:account) }
      it { should validate_presence_of(:call_flow) }
      it { should validate_presence_of(:name) }
      it { should validate_uniqueness_of(:name).scoped_to(:account_id) }
    end

    context "call" do
      let (:channel) { a_subclass.make }
      let (:queued_call) { channel.reload.queued_calls.first }

      it "call ok" do
        broker_client.should_receive(:notify_call_queued).with(channel.id)

        call_log = channel.call 'foo'
        call_log.state.should == :queued
        call_log.address.should == 'foo'

        queued_calls = channel.queued_calls
        queued_calls.length.should == 1
        queued_calls[0].address.should == 'foo'
        queued_calls[0].call_log_id.should == call_log.id
      end

      it "call raises" do
        broker_client.should_receive(:notify_call_queued).with(channel.id).and_raise("Oh no!")

        call_log = channel.call 'foo'
        call_log.state.should == :failed
      end

      it "call and set direction outgoing" do
        broker_client.should_receive(:notify_call_queued)

        call_log = channel.call 'foo'
        call_log.direction.should == :outgoing
      end

      it "call with custom callback url" do
        broker_client.should_receive(:notify_call_queued)

        channel.call 'foo', :callback_url => 'bar'
        queued_call.callback_url.should == 'bar'
      end

      it "call with custom flow" do
        broker_client.should_receive(:notify_call_queued)
        channel.call 'foo', :flow => Compiler.make { Answer(); Hangup() }
        queued_call.flow.should == Compiler.make { Answer(); Hangup() }
      end

      it "call with custom status callback url" do
        broker_client.should_receive(:notify_call_queued)

        channel.call 'foo', :status_callback_url => 'bar'
        queued_call.status_callback_url.should == 'bar'
      end

      it "notify with time when scheduling delayed call" do
        time = Time.now.utc + 1.hour
        broker_client.should_receive(:notify_call_queued).with(channel.id, time)
        channel.call 'foo', :not_before => time
      end

      it "notify with time when scheduling delayed call with time as string" do
        time = Time.now.utc + 1.hour
        broker_client.should_receive(:notify_call_queued).with(channel.id, time).once
        channel.call 'foo', :not_before => time.to_s
      end

      it "obey queue lower time bound" do
        schedule = channel.project.schedules.make :time_from => '10:00', :time_to => '12:00'
        broker_client.should_receive(:notify_call_queued)
        channel.call 'foo', :not_before => '2012-12-20T08:00:00Z', :schedule_id => schedule.id
        queued_call.not_before.should == '2012-12-20T10:00:00Z'
      end

      it "obey queue upper time bound" do
        schedule = channel.project.schedules.make :time_from => '10:00', :time_to => '12:00'
        broker_client.should_receive(:notify_call_queued)
        channel.call 'foo', :not_before => '2012-12-20T13:00:00', :schedule_id => schedule.id
        queued_call.not_before.should == '2012-12-21T10:00:00'
      end

      it "uses selected time zone for 'not before' date" do
        broker_client.should_receive(:notify_call_queued)
        channel.call_flow.project.update_attribute :time_zone, 'Buenos Aires'
        channel.reload.call 'foo', :not_before => '2012-12-20T10:00:00', :time_zone => 'Paris'
        queued_call.not_before.should eq(Time.parse('2012-12-20T09:00:00 UTC'))
      end

      it "uses project's time zone for 'not before' date" do
        broker_client.should_receive(:notify_call_queued)
        channel.call_flow.project.update_attribute :time_zone, 'Buenos Aires'
        channel.reload.call 'foo', :not_before => '2012-12-20T10:00:00'
        queued_call.not_before.should eq(Time.parse('2012-12-20T13:00:00 UTC'))
      end

    end

    it "call create_channel on broker_client when create" do
      channel = a_subclass.make_unsaved
      broker_client.should_receive(:create_channel).with do |channel_id|
        channel_id == channel.id
      end
      channel.save!
    end

    it "call delete_channel and create_channel on broker_client when update" do
      broker_client.should_receive(:create_channel)
      channel = a_subclass.make

      broker_client.should_receive(:delete_channel).with(channel.id).ordered
      broker_client.should_receive(:create_channel).with(channel.id).ordered

      channel.save!
    end

    it "call delete_channel on broker_client when destroy" do
      channel = a_subclass.make
      broker_client.should_receive(:delete_channel).with(channel.id)
      channel.destroy
    end

    it "register? and_return true" do
      channel = a_subclass.new :config => { 'register' => '1' }
      channel.register?.should_not be_nil
    end

    it "register? and_return false" do
      channel = a_subclass.new :config => { 'register' => '0' }
      channel.register?.should be_false
    end

    context "poll call" do
      it "return nil if no queued calls" do
        channel = a_subclass.make
        channel.poll_call.should == nil
      end

      it "return queued call and destroy it" do
        channel = a_subclass.make
        queued_call = channel.queued_calls.make

        channel.poll_call.should == queued_call
        QueuedCall.count.should == 0
      end

      it "not return scheduled calls in the future" do
        channel = a_subclass.make
        channel.queued_calls.make :not_before => Time.now + 1.hour

        channel.poll_call.should == nil
      end
    end

    it "create new session without a call log" do
      channel = a_subclass.make
      session = channel.new_session
      session.call_log.account.should == channel.account
      session.call_log.project.should == channel.call_flow.project
      session.call_log.channel.should == channel
      session.call_log.direction.should == :incoming
      session.call_log.state.should == :active
    end
  end

  it "should assign a guid" do
    channel = Channels::Voxeo.make_unsaved
    channel.guid.should be_nil
    channel.save!
    channel.guid.should_not be_nil
  end

  context 'broker client' do

    let(:local_pbx_port) { Rails.configuration.verboice_configuration[:local_pbx_broker_port].to_i }
    let(:voxeo_port) { Rails.configuration.verboice_configuration[:voxeo_broker_port].to_i }

    it 'should return voxeo broker client for Channels::Voxeo' do
      channel = Channels::Voxeo.new
      BrokerClient.should_receive(:new).with(voxeo_port).and_return(broker_client)
      channel.broker_client.should eq(broker_client)
    end

    Channel.all_leaf_subclasses.reject{|a_subclass| a_subclass == Channels::Voxeo }.each do |a_subclass|
      it "should return local broker client for #{a_subclass}" do
        channel = a_subclass.new
        BrokerClient.should_receive(:new).with(local_pbx_port).and_return(broker_client)
        channel.broker_client.should eq(broker_client)
      end
    end

  end
end
