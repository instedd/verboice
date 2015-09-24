require 'spec_helper'

describe Telemetry::StepsPerCallFlowCollector do

  let(:channel_1) { Channels::SipServer.make }
  let(:channel_2) { Channels::Twilio.make }

  it "builds counters for calls per day per channel" do
    5.times { CallLog.make started_at: Time.utc(2015, 1, 1, 13, 0, 0), channel: channel_1, state: :completed }
    3.times { CallLog.make started_at: Time.utc(2015, 1, 1, 15, 0, 0), channel: channel_1, state: :failed }
    8.times { CallLog.make started_at: Time.utc(2015, 1, 1, 18, 0, 0), channel: channel_2, state: :completed }


    3.times { CallLog.make channel: channel_1, started_at: Time.utc(2015, 1, 2, 12, 0, 0), state: :failed }
    4.times { CallLog.make channel: channel_2, started_at: Time.utc(2015, 1, 2, 18, 0, 0), state: :completed }

    current_stats.should eq({
      "counters" => [
        {
          "metric" => "calls",
          "key" => { "date" => Date.new(2015,01,1).iso8601, "channel_id" => channel_1.id, "state" => "completed" },
          "value" => 5
        },
        {
          "metric" => "calls",
          "key" => { "date" => Date.new(2015,01,1).iso8601, "channel_id" => channel_1.id, "state" => "failed" },
          "value" => 3
        },
        {
          "metric" => "calls",
          "key" => { "date" => Date.new(2015,01,1).iso8601, "channel_id" => channel_2.id, "state" => "completed" },
          "value" => 8
        },
        {
          "metric" => "calls",
          "key" => { "date" => Date.new(2015,01,2).iso8601, "channel_id" => channel_1.id, "state" => "failed" },
          "value" => 3
        },
        {
          "metric" => "calls",
          "key" => { "date" => Date.new(2015,01,2).iso8601, "channel_id" => channel_2.id, "state" => "completed" },
          "value" => 4
        }
      ]
    })
  end

  it "doesn't count queued or active calls" do
    3.times { CallLog.make started_at: Time.now, channel: channel_1, state: :active }
    4.times { CallLog.make started_at: Time.now, channel: channel_1, state: :completed }
    5.times { CallLog.make started_at: Time.now, channel: channel_1, state: :queued }

    current_stats["counters"].should have(1).item

    counter = current_stats["counters"][0].tap do |c|
      c["value"].should be(4)
      c["key"]["state"].should eq("completed")
      c["key"]["channel_id"].should be(channel_1.id)
    end
  end

  def current_stats
    period  = InsteddTelemetry.current_period
    Telemetry::CallsPerDayPerChannelCollector.collect_stats(p)
  end

end
