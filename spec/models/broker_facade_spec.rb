require 'spec_helper'

describe BrokerFacade do
  before(:each) do
    BaseBroker.instance = mock 'broker'
    @facade = BrokerFacade.new 1
    @channel = mock 'channel'
    Channel.should_receive(:find).with(:channel_id).and_return @channel
  end

  [:notify_call_queued, :create_channel, :delete_channel].each do |method|
    it "#{method}" do
      BaseBroker.instance.should_receive(method).with @channel
      @facade.send method, :channel_id
    end
  end
end
