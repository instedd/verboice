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
    allow(BrokerClient).to receive(:create_channel)
  end

  after(:each) do
    allow(BrokerClient).to receive(:destroy_channel)
    DatabaseCleaner.clean_with(:truncation)
    Timecop.return
  end

  Channel.all_leaf_subclasses.each do |a_channel|

    context "validations" do
      before(:each) { a_channel.make }

      it { is_expected.to belong_to(:account) }
      it { is_expected.to belong_to(:call_flow) }

      it { is_expected.to validate_presence_of(:account) }
      it { is_expected.to validate_presence_of(:name) }
      it { is_expected.to validate_uniqueness_of(:name).scoped_to(:account_id) }
    end

    context "call" do
      let (:channel) { a_channel.make }
      let (:queued_call) { channel.reload.queued_calls.first }

      it "call ok" do
        expect(BrokerClient).to receive(:notify_call_queued).with(channel.id)

        call_log = channel.call 'foo'
        expect(call_log.state).to eq(:queued)
        expect(call_log.address).to eq('foo')

        queued_calls = channel.queued_calls
        expect(queued_calls.length).to eq(1)
        expect(queued_calls[0].address).to eq('foo')
        expect(queued_calls[0].call_log_id).to eq(call_log.id)
      end

      it "call raises" do
        expect(BrokerClient).to receive(:notify_call_queued).with(channel.id).and_raise("Oh no!")

        call_log = channel.call 'foo'
        expect(call_log.state).to eq(:queued)
      end

      it "call and set direction outgoing" do
        expect(BrokerClient).to receive(:notify_call_queued)

        call_log = channel.call 'foo'
        expect(call_log.direction).to eq(:outgoing)
      end

      it "call with custom callback url" do
        expect(BrokerClient).to receive(:notify_call_queued)

        channel.call 'foo', :callback_url => 'bar'
        expect(queued_call.callback_url).to eq('bar')
        expect(queued_call.call_flow).to be_nil
      end

      it "call with custom flow" do
        expect(BrokerClient).to receive(:notify_call_queued)
        channel.call 'foo', :flow => %(<Response><Hangup/></Response>)
        expect(queued_call.flow).to eq(%(<Response><Hangup/></Response>))
      end

      it "call with custom status callback url" do
        expect(BrokerClient).to receive(:notify_call_queued)

        channel.call 'foo', :status_callback_url => 'bar'
        expect(queued_call.status_callback_url).to eq('bar')
      end

      it "notify with time when scheduling delayed call" do
        time = Time.now.utc + 1.hour
        expect(BrokerClient).to receive(:notify_call_queued).with(channel.id, time)
        channel.call 'foo', :not_before => time
      end

      it "notify with time when scheduling delayed call with time as string" do
        time = Time.now.utc + 1.hour
        expect(BrokerClient).to receive(:notify_call_queued).with(channel.id, time).once
        channel.call 'foo', :not_before => time.to_s
      end

      it "obey queue lower time bound" do
        schedule = channel.project.schedules.make :time_from => '10:00', :time_to => '12:00'
        expect(BrokerClient).to receive(:notify_call_queued)
        channel.call 'foo', :not_before => '2012-12-20T08:00:00Z', :schedule_id => schedule.id
        expect(queued_call.not_before).to eq('2012-12-20T10:00:00Z')
      end

      it "obey queue upper time bound" do
        schedule = channel.project.schedules.make :time_from => '10:00', :time_to => '12:00'
        expect(BrokerClient).to receive(:notify_call_queued)
        channel.call 'foo', :not_before => '2012-12-20T13:00:00', :schedule_id => schedule.id
        expect(queued_call.not_before).to eq('2012-12-21T10:00:00')
      end

      it "uses selected time zone for 'not before' date" do
        expect(BrokerClient).to receive(:notify_call_queued)
        channel.call_flow.project.update_attribute :time_zone, 'Buenos Aires'
        channel.reload.call 'foo', :not_before => '2012-12-20T10:00:00', :time_zone => 'Paris'
        expect(queued_call.not_before).to eq(Time.parse('2012-12-20T09:00:00 UTC'))
      end

      it "uses project's time zone for 'not before' date" do
        expect(BrokerClient).to receive(:notify_call_queued)
        channel.call_flow.project.update_attribute :time_zone, 'Buenos Aires'
        channel.reload.call 'foo', :not_before => '2012-12-20T10:00:00'
        expect(queued_call.not_before).to eq(Time.parse('2012-12-20T13:00:00 UTC'))
      end

      it "uses project's time zone when not before is nil" do
        expect(BrokerClient).to receive(:notify_call_queued)
        channel.call_flow.project.update_attribute :time_zone, 'Buenos Aires'
        channel.reload.call 'foo'
        expect(queued_call.time_zone).to eq('America/Argentina/Buenos_Aires')
      end

      it "calls with variables" do
        expect(BrokerClient).to receive(:notify_call_queued)
        call_log = channel.call 'foo', vars: {'bar' => '1', 'baz' => 'eee'}
        expect(queued_call.variables).to eq({'bar' => 1, 'baz' => 'eee'})
      end

      it "calls with numeric-like variables" do
        expect(BrokerClient).to receive(:notify_call_queued)
        call_log = channel.call 'foo', vars: {'bar' => '123', 'quux' => '0001', 'zero' => '0'}
        expect(queued_call.variables).to eq({'bar' => 123, 'quux' => '0001', 'zero' => 0})
      end

      it "stores contact and schedule call" do
        channel.call 'foo', contact_id: 7, scheduled_call_id: 13
        expect(queued_call.contact_id).to eq(7)
        expect(queued_call.scheduled_call_id).to eq(13)
      end
    end

    it "call create_channel on broker client when create" do
      channel = a_channel.make_unsaved
      expect(BrokerClient).to receive(:create_channel) do |channel_id|
        channel_id == channel.id
      end
      channel.save!
    end

    it "disable cancels queued calls" do
      channel = a_channel.make

      call_log = channel.call 'foo'
      expect(channel.queued_calls.count).to eq(1)

      channel.disable!

      expect(channel).not_to be_enabled
      expect(channel.reload.queued_calls).to be_empty
      expect(call_log.state).to eq(:cancelled)
    end

    if a_channel.is_a?(Channels::Sip)
      it "register? and_return true" do
        channel = a_channel.new :config => { 'register' => 'true' }
        expect(channel.register?).not_to be_nil
      end

      it "register? and_return false" do
        channel = a_channel.new
        expect(channel.register?).to be(false)
      end
    end
  end

  it "should assign a guid" do
    channel = Channels::Voxeo.make_unsaved
    expect(channel.guid).to be_nil
    channel.save!
    expect(channel.guid).not_to be_nil
  end
end
