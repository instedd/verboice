require 'spec_helper'

describe Telemetry::CallsPerDayPerChannelCollector do

  let(:channel_1) { Channels::SipServer.make }
  let(:channel_2) { Channels::Twilio.make }

  it "builds counters for calls per day per channel" do
    d0 = DateTime.new(2015,1,1,0,0,0)
    d1 = d0 + InsteddTelemetry::Period.span

    Timecop.freeze(d0)

    5.times { CallLog.make started_at: Time.utc(2015, 1, 1, 13, 0, 0), channel: channel_1, state: :completed }
    3.times { CallLog.make started_at: Time.utc(2015, 1, 1, 15, 0, 0), channel: channel_1, state: :failed }
    8.times { CallLog.make started_at: Time.utc(2015, 1, 1, 18, 0, 0), channel: channel_2, state: :completed }


    3.times { CallLog.make channel: channel_1, started_at: Time.utc(2015, 1, 2, 12, 0, 0), state: :failed }
    4.times { CallLog.make channel: channel_2, started_at: Time.utc(2015, 1, 2, 18, 0, 0), state: :completed }

    period = InsteddTelemetry::Period.current

    # call log made after period
    Timecop.freeze(d1)
    CallLog.make channel: channel_1, started_at: Time.utc(2015, 1, 2, 12, 0, 0), state: :completed

    stats(period).should eq({
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

    period = InsteddTelemetry::Period.current
    counters = stats(period)["counters"]
    counters.should have(1).item

    counter = counters[0].tap do |c|
      c["value"].should be(4)
      c["key"]["state"].should eq("completed")
      c["key"]["channel_id"].should be(channel_1.id)
    end
  end

  it "doesn't count calls before the period" do
    to = Time.now
    from = to - 1.week
    period = InsteddTelemetry::Period.new beginning: from, end: to

    CallLog.make started_at: to - 1.day, created_at: to - 1.day, channel: channel_1, state: :completed
    CallLog.make started_at: to - 4.days, created_at: to - 4.days, channel: channel_1, state: :completed

    CallLog.make started_at: from - 1.day, created_at: from - 1.day, channel: channel_1, state: :completed
    CallLog.make started_at: from - 5.days, created_at: from - 5.days, channel: channel_1, state: :completed

    stats(period).should eq({
      "counters" => [
        {
          "metric" => "calls",
          "key" => { "date" => (to - 4.days).to_date.iso8601, "channel_id" => channel_1.id, "state" => "completed" },
          "value" => 1
        },
        {
          "metric" => "calls",
          "key" => { "date" => (to - 1.day).to_date.iso8601, "channel_id" => channel_1.id, "state" => "completed" },
          "value" => 1
        }
      ]
    })
  end

  def stats(period)
    Telemetry::CallsPerDayPerChannelCollector.collect_stats(period)
  end

end
