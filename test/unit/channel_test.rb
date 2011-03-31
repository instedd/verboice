require 'test_helper'

class ChannelTest < ActiveSupport::TestCase

  # Because we have after_commit callbacks...
  self.use_transactional_fixtures = false

  should belong_to(:account)
  should belong_to(:application)

  should validate_presence_of(:account)
  should validate_presence_of(:application)

  teardown do
    PbxClient.stubs(:delete_channel)
    Account.destroy_all
  end

  test "call PbxClient.update_channel on create" do
    channel = Channel.make_unsaved
    PbxClient.expects(:update_channel).with do |channel_id|
      channel_id == channel.id
    end
    channel.save!
  end

  test "call PbxClient.update_channel on update" do
    PbxClient.expects(:update_channel)
    channel = Channel.make

    PbxClient.expects(:update_channel).with do |channel_id|
      channel_id == channel.id
    end
    channel.save!
  end

  test "call PbxClient.delete_channel on destroy" do
    channel = Channel.make
    PbxClient.expects(:delete_channel).with do |channel_id|
      channel_id == channel.id
    end
    channel.destroy
  end
end
