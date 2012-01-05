require 'test_helper'

class BaseBrokerTest < ActiveSupport::TestCase
  setup do
    @broker = BaseBroker.new
    @broker.stubs(:pbx_available? => true)
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

    should "send ringing notification" do
      queued_call = @channel.queued_calls.make

      @broker.expects(:call).with { |session| session.expects(:notify_status).with('ringing') }
      @broker.notify_call_queued @channel
    end

    should "close session if call fails" do
      queued_call = @channel.queued_calls.make
      the_session = nil

      @broker.expects(:call).with { |session| the_session = session }.raises Exception.new
      @broker.notify_call_queued @channel

      assert_equal 0, @broker.sessions.length
      assert_equal 0, @broker.active_calls_count_for(@channel)
    end

    should "requeue call if pbx unavailable on call" do
      queued_call = @channel.queued_calls.make
      the_session = nil

      @broker.expects(:call).with { |session| the_session = session }.raises PbxUnavailableException.new
      @broker.notify_call_queued @channel

      assert_equal [queued_call.attributes.except('id', 'created_at', 'updated_at')], @channel.queued_calls.all.map{|x| x.attributes.except('id', 'created_at', 'updated_at')}.to_a

      queued_call = @channel.queued_calls.first
      assert_equal :queued, queued_call.call_log.state

      assert_equal 0, @broker.sessions.length
      assert_equal 0, @broker.active_calls_count_for(@channel)
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
  end

  context "finish session" do
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

    should "send status callback on successfull" do
      queued_call = @channel.queued_calls.make
      the_session = nil

      @broker.expects(:call).with { |session| the_session = session }
      @broker.notify_call_queued @channel

      the_session.application.status_callback_url = 'http://foo'
      the_session.pbx = mock('pbx')
      the_session.expects(:notify_status).with('completed')
      @broker.finish_session_successfully the_session
    end

    should "send status callback on failure" do
      queued_call = @channel.queued_calls.make
      the_session = nil

      @broker.expects(:call).with { |session| the_session = session }
      @broker.notify_call_queued @channel

      the_session.application.status_callback_url = 'http://foo'
      the_session.pbx = mock('pbx')
      the_session.expects(:notify_status).with('failed')
      @broker.finish_session_with_error the_session, 'An error'
    end
  end

  context "accept call" do
    should "run when there is already a session" do
      @channel.application.flow = [:answer, :hangup]
      @channel.application.save!

      queued_call = @channel.queued_calls.make
      the_session = nil
      @broker.expects(:call).with { |session| the_session = session }
      @broker.notify_call_queued @channel

      pbx = stub 'pbx', :session_id => the_session.id
      pbx.expects :answer
      pbx.expects(:hangup).twice

      EM.expects(:fiber_sleep).with 2

      @broker.accept_call pbx

      assert_equal queued_call.address, the_session.call_log.address
      assert_equal :completed, the_session.call_log.state
      assert_equal 0, @broker.sessions.length
      assert_equal 0, @broker.active_calls[@channel.id].length
    end

    should "run when there is no session" do
      @channel.application.flow = [:answer, :hangup]
      @channel.application.save!

      pbx = stub 'pbx', :session_id => nil, :channel_id => @channel.id, :caller_id => '1234'
      pbx.expects :answer
      pbx.expects(:hangup).twice

      EM.expects(:fiber_sleep).with 2

      @broker.accept_call pbx

      call_logs = CallLog.all
      assert_equal 1, call_logs.length
      call_log = call_logs.first

      assert_equal '1234', call_log.address
      assert_equal :completed, call_log.state
      assert_equal 0, @broker.sessions.length
      assert_equal 0, @broker.active_calls[@channel.id].length
    end

    should "resume when there is a suspended session" do
      session = Session.new
      session.suspend
      @broker.sessions['id'] = session

      pbx = stub 'pbx', :session_id => 'id'

      session.expects(:resume)
      @broker.accept_call pbx
    end

    should "resume and close pbx connection" do
      @channel.application.flow = [:yield, :hangup]
      @channel.application.save!

      pbx = stub 'pbx', :session_id => nil, :channel_id => @channel.id, :caller_id => '1234'

      f = Fiber.new do
        @broker.accept_call pbx
      end

      # This will start the session and yield at the YieldCommand
      f.resume

      pbx.expects :close_connection
      session = @broker.sessions.values.first
      @broker.expects(:restart).with(session)
      @broker.redirect session.call_log.id, :flow => [:hangup]

      # Resume the session and yields because it is suspended
      f.resume

      pbx2 = stub 'pbx2', :session_id => session.id, :channel_id => nil, :caller_id => '1234'
      pbx2.expects(:hangup).twice
      EM.expects(:fiber_sleep).with 2

      @broker.accept_call pbx2
    end

    should "send 'in-progress' status notification" do
      @channel.application.flow = [:answer]
      @channel.application.save!

      class << @broker
        def find_or_create_session(pbx)
          session = super
          session.expects(:notify_status).with('in-progress')
          session.expects(:notify_status).with('completed')
          session
        end
      end

      pbx = stub 'pbx', :session_id => nil, :channel_id => @channel.id, :caller_id => '1234'
      pbx.expects :answer
      pbx.expects :hangup

      EM.expects(:fiber_sleep).with 2

      @broker.accept_call pbx
    end
  end
end

class YieldCommand
  def run(session)
    Fiber.yield
  end
end

