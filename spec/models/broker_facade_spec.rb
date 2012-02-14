require 'spec_helper'

describe BrokerFacade do
  before(:each) { BaseBroker.instance = mock 'broker' }
  let(:facade) { BrokerFacade.new 1 }
  let(:channel) { Channel.make }

  [:notify_call_queued, :create_channel, :delete_channel].each do |method|
    it "#{method}" do
      BaseBroker.instance.should_receive(method).with channel
      facade.send method, channel.id
    end
  end

  it "schedule delayed call" do
    time = Time.now.utc + 2.hours
    BaseBroker.instance.should_receive(:schedule_call).with channel.id, time
    facade.notify_call_queued channel.id,  time
  end
end
