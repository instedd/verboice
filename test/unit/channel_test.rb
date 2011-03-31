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

  test "call PbxClient.update_channel on save" do
    channel = Channel.make_unsaved
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

  context 'host and port' do

    setup do
      @channel_with_host_and_port = Channel.new :config => { 'host_and_port' => 'host:1234' }
      @channel_without_host_and_port = Channel.new
    end

    should "return true for host_and_port?" do
      assert @channel_with_host_and_port.host_and_port?
    end

    should "return false for host_and_port?" do
      assert !@channel_without_host_and_port.host_and_port?
    end

    should "return host_and_port" do
      assert_equal ['host', '1234'], @channel_with_host_and_port.host_and_port
    end

  end

  test "register? returns true" do
    channel = Channel.new :config => { 'register' => '1' }
    assert channel.register?
  end

  test "register? returns false" do
    channel = Channel.new :config => { 'register' => '0' }
    assert !channel.register?
  end
end
