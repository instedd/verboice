require 'test_helper'

class ChannelTest < ActiveSupport::TestCase
  # Because we have after_commit callbacks...
  self.use_transactional_fixtures = false

  teardown do
    BrokerClient.stubs(:delete_channel)
    [Account, Channel, CallLog, QueuedCall].each &:destroy_all
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
      BrokerClient.expects(:notify_call_queued).with(@channel.id)

      call_log = @channel.call 'foo'
      assert_equal :queued, call_log.state
      assert_equal 'foo', call_log.address

      queued_calls = @channel.queued_calls
      assert_equal 1, queued_calls.length
      assert_equal 'foo', queued_calls[0].address
      assert_equal call_log.id, queued_calls[0].call_log_id
    end

    should "call raises" do
      BrokerClient.expects(:notify_call_queued).with(@channel.id).raises("Oh no!")

      call_log = @channel.call 'foo'
      assert_equal :failed, call_log.state
    end

    should "call and set direction outgoing" do
      BrokerClient.expects(:notify_call_queued)

      call_log = @channel.call 'foo'
      assert_equal :outgoing, call_log.direction
    end

    should "call with custom callback url" do
      BrokerClient.expects(:notify_call_queued)

      @channel.call 'foo', :callback_url => 'bar'
      queued_call = @channel.queued_calls.first
      assert_equal 'bar', queued_call.callback_url
    end

    should "call with custom flow" do
      BrokerClient.expects(:notify_call_queued)
      @channel.call 'foo', :flow => [:answer, :hangup]
      queued_call = @channel.queued_calls.first
      assert_equal [:answer, :hangup], queued_call.flow
    end

    should "call with custom status callback url" do
      BrokerClient.expects(:notify_call_queued)

      @channel.call 'foo', :status_callback_url => 'bar'
      queued_call = @channel.queued_calls.first
      assert_equal 'bar', queued_call.status_callback_url
    end
  end

  test "call BrokerClient.create_channel on create" do
    channel = Channel.make_unsaved
    BrokerClient.expects(:create_channel).with do |channel_id|
      channel_id == channel.id
    end
    channel.save!
  end

  test "call BrokerClient.delete_channel and BrokerClient.create_channel on update" do
    BrokerClient.expects(:create_channel)
    channel = Channel.make

    seq = sequence('seq')
    BrokerClient.expects(:delete_channel).with(channel.id).in_sequence(seq)
    BrokerClient.expects(:create_channel).with(channel.id).in_sequence(seq)

    channel.save!
  end

  test "call BrokerClient.delete_channel on destroy" do
    channel = Channel.make
    BrokerClient.expects(:delete_channel).with(channel.id)
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

  context "poll call" do
    should "return nil if no queued calls" do
      channel = Channel.make
      assert_nil channel.poll_call
    end

    should "return queued call and destroy it" do
      channel = Channel.make
      queued_call = channel.queued_calls.make

      assert_equal queued_call, channel.poll_call
      assert_equal 0, QueuedCall.count
    end

    should "not return scheduled calls in the future" do
      channel = Channel.make
      channel.queued_calls.make :not_before => Time.now + 1.hour

      assert_nil channel.poll_call
    end
  end

  test "create new session without a call log" do
    channel = Channel.make
    session = channel.new_session
    assert_equal channel.account, session.call_log.account
    assert_equal channel.application, session.call_log.application
    assert_equal channel, session.call_log.channel
    assert_equal :incoming, session.call_log.direction
    assert_equal :active, session.call_log.state
  end
end
