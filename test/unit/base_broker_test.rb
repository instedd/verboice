require 'test_helper'

class BaseBrokerTest < ActiveSupport::TestCase
  setup do
    @broker = BaseBroker.new
    @channel = Channel.make
  end

  context "notify call queued" do
    should "not call if there is no queued call" do
      @broker.expects(:call).never
      @broker.notify_call_queued @channel
    end

    should "call if there is a queued call" do
      queued_call = @channel.queued_calls.make
      the_session = nil

      @broker.expects(:call).with { |session| the_session = session }
      @broker.notify_call_queued @channel

      assert_equal 1, @broker.sessions.length
      assert_equal the_session, @broker.sessions[the_session.id]
      assert_equal 1, @broker.active_calls_count_for(@channel)
      assert_equal the_session, @broker.active_calls[@channel.id][the_session.id]
    end

    should "not call if limit reached" do
      @channel.limit = 1

      queued_call_1 = @channel.queued_calls.make
      queued_call_2 = @channel.queued_calls.make

      the_session = nil
      @broker.expects(:call).once.with { |session| the_session = session }
      @broker.notify_call_queued @channel

      @broker.notify_call_queued @channel
    end

    should "finish session successfully" do
      queued_call = @channel.queued_calls.make
      the_session = nil

      @broker.expects(:call).with { |session| the_session = session }
      @broker.notify_call_queued @channel

      @broker.finish_session_successfully the_session
      assert_equal 0, @broker.sessions.length
      assert_equal 0, @broker.active_calls[@channel.id].length
      assert_equal :completed, the_session.call_log.state
    end

    should "finish session with error" do
      queued_call = @channel.queued_calls.make
      the_session = nil

      @broker.expects(:call).with { |session| the_session = session }
      @broker.notify_call_queued @channel

      @broker.finish_session_with_error the_session, 'An error'
      assert_equal 0, @broker.sessions.length
      assert_equal 0, @broker.active_calls[@channel.id].length
      assert_match /An error/, the_session.call_log.details
      assert_equal :failed, the_session.call_log.state
    end
  end
end
