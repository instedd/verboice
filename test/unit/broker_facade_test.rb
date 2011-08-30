require 'test_helper'

class BrokerFacadeTest < ActiveSupport::TestCase
  setup do
    BaseBroker.instance = mock 'broker'
    @facade = BrokerFacade.new 1
    @channel = mock 'channel'
    Channel.expects(:find).with(:channel_id).returns @channel
  end

  [:notify_call_queued, :create_channel, :delete_channel].each do |method|
    should "#{method}" do
      BaseBroker.instance.expects(method).with @channel
      @facade.send method, :channel_id
    end
  end
end
