require 'test_helper'

class ChannelTest < ActiveSupport::TestCase
  should belong_to(:account)
  should belong_to(:application)

  should validate_presence_of(:account)
  should validate_presence_of(:application)

  test "call PbxClient.update_channel on save" do
    channel = Channel.make_unsaved
    PbxClient.expects(:update_channel).with do |channel_id|
      channel_id == channel.id
    end
    channel.save
  end
end
