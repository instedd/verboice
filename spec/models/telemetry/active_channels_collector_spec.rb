require 'spec_helper'

describe Telemetry::ActiveChannelsCollector do

  let(:to) { Time.now }
  let(:from) { to - 1.week }
  let(:period) { InsteddTelemetry::Period.new(beginning: from, end: to) }

  it 'counts active channels' do
    channel_1 = Channels::Twilio.make created_at: to - 1.day
    channel_2 = Channels::SipServer.make created_at: to - 30.days
    channel_3 = Channels::CustomSip.make created_at: to + 1.day
    channel_4 = Channels::Twilio.make created_at: to - 5.days

    CallLog.make channel: channel_1, state: 'completed'

    CallLog.make channel: channel_2, state: 'cancelled'
    CallLog.make channel: channel_2, state: 'active'
    CallLog.make channel: channel_2, state: 'completed'

    CallLog.make channel: channel_3, state: 'completed'

    CallLog.make channel: channel_4, state: 'failed'

    stats = Telemetry::ActiveChannelsCollector.collect_stats period

    stats.should eq({
      counters: [{
        metric: 'active_channels',
        key: {},
        value: 2
      }]
    })
  end

end
