require 'test_helper'

class ChannelTest < ActiveSupport::TestCase

  # Because we have after_commit callbacks...
  self.use_transactional_fixtures = false

  teardown do
    PbxClient.stubs(:delete_channel)
    Account.destroy_all
  end

  context "validations" do
    setup { Channel.make }

    should belong_to(:account)
    should belong_to(:application)

    should validate_presence_of(:account)
    should validate_presence_of(:application)
    should validate_presence_of(:name)
    should validate_uniqueness_of(:name).scoped_to(:account_id)
  end

  context "call" do
    setup do
      @channel = Channel.make
    end

    should "call ok" do
      PbxClient.expects(:call).with do |address, channel_id, call_log_id|
        @the_call_log_id = call_log_id
        address == 'foo' && channel_id == @channel.id
      end

      call_log = @channel.call 'foo'
      assert_equal @the_call_log_id, call_log.id
      assert_equal :active, call_log.state
      assert_equal 'foo', call_log.address
    end

    should "call raises" do
      PbxClient.expects(:call).with do |address, channel_id, call_log_id|
        @the_call_log_id = call_log_id
        address == 'foo' && channel_id == @channel.id
      end.raises("Oh no!")

      call_log = @channel.call 'foo'
      assert_equal @the_call_log_id, call_log.id
      assert_equal :failed, call_log.state
    end

    should "call and set direction outgoing" do
      PbxClient.expects(:call)

      call_log = @channel.call 'foo'
      assert_equal :outgoing, call_log.direction
    end
  end

  test "call PbxClient.create_channel on create" do
    channel = Channel.make_unsaved
    PbxClient.expects(:create_channel).with do |channel_id|
      channel_id == channel.id
    end
    channel.save!
  end

  test "call PbxClient.delete_channel and PbxClient.create_channel on update" do
    PbxClient.expects(:create_channel)
    channel = Channel.make

    seq = sequence('seq')
    PbxClient.expects(:delete_channel).with(channel.id).in_sequence(seq)
    PbxClient.expects(:create_channel).with(channel.id).in_sequence(seq)

    channel.save!
  end

  test "call PbxClient.delete_channel on destroy" do
    channel = Channel.make
    PbxClient.expects(:delete_channel).with(channel.id)
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
