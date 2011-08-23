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
  
  context "can call" do
    
    setup do
      @channel = Channel.make
    end
    
    should "tell true if channel has no limit" do
      @channel.expects(:has_limit?).returns(false)
      assert_true @channel.can_call?
    end
    
    should "tell true if active calls are inside limit" do
      @channel.expects(:has_limit?).returns(true)
      @channel.expects(:limit).returns(5)
      
      1.upto(4).each{ CallLog.make :channel => @channel, :started_at => Time.now, :finished_at => nil }
      
      assert_true @channel.can_call?
    end
    
    should "tell false if limit is exceeded" do
      @channel.expects(:has_limit?).returns(true)
      @channel.expects(:limit).returns(5)
      
      1.upto(5).each{ CallLog.make :channel => @channel, :started_at => Time.now, :finished_at => nil }
      
      assert_false @channel.can_call?
    end
    
  end

  context "call" do
    setup do
      @channel = Channel.make
    end

    should "call ok" do
      PbxClient.expects(:try_call_from_queue).with do |channel_id|
        # @the_call_log_id = call_log_id
        # address == 'foo' && channel_id == @channel.id
        channel_id == @channel.id
      end

      call_log = @channel.call 'foo'
      # assert_equal @the_call_log_id, call_log.id
      assert_equal :active, call_log.state
      assert_equal 'foo', call_log.address
    end

    should "call raises" do
      PbxClient.expects(:try_call_from_queue).with do |channel_id|
        # @the_call_log_id = call_log_id
        # address == 'foo' && channel_id == @channel.id
        channel_id == @channel.id
      end.raises("Oh no!")

      call_log = @channel.call 'foo'
      # assert_equal @the_call_log_id, call_log.id
      assert_equal :failed, call_log.state
    end

    should "call and set direction outgoing" do
      PbxClient.expects(:try_call_from_queue)

      call_log = @channel.call 'foo'
      assert_equal :outgoing, call_log.direction
    end
    
    should "enqueue call" do
      PbxClient.stubs(:try_call_from_queue)
      CallQueue.expects(:enqueue).with do |channel, call_log, address|
        @the_call_log = call_log
        address == 'foo' && channel.id == @channel.id
      end
      
      call_log = @channel.call 'foo'
      assert_equal @the_call_log, call_log
    end
    
    should "destroy enqueued call if exception is raised" do
      call = mock('mock')
      PbxClient.expects(:try_call_from_queue).raises
      CallQueue.expects(:enqueue).returns(call)
      call.expects(:destroy)
      
      @channel.call 'foo'
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
