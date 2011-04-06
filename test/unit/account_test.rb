require 'test_helper'

class AccountTest < ActiveSupport::TestCase
  should have_many(:applications)
  should have_many(:channels)
  should have_many(:call_logs)

  context "call" do
    setup do
      @account = Account.make
      @account.stubs(:channels => mock('channels'))
      @channel = mock('channel')
      @seq = sequence('seq')
    end

    should "call channel" do
      @account.channels.expects(:find_by_name!).with('some_channel').returns(@channel).in_sequence(@seq)
      @channel.expects(:call).with('1234').in_sequence(@seq)

      @account.call :channel => 'some_channel', :address => '1234'
    end
  end
end
