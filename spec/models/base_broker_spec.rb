require 'spec_helper'

describe BaseBroker do
  before(:each) do
    @broker = BaseBroker.new
    @broker.stub(:pbx_available? => true)
    @channel = Channel.make
  end

  context "notify call queued" do
    it "not call if there is no queued call" do
      @broker.should_receive(:call).never
      @broker.notify_call_queued @channel
    end

    it "call if there is a queued call" do
      queued_call = @channel.queued_calls.make
      the_session = nil

      @broker.should_receive(:call).with { |session| the_session = session }
      @broker.notify_call_queued @channel

      @broker.sessions.length.should == 1
      @broker.sessions[the_session.id].should == the_session
      @broker.active_calls_count_for(@channel).should == 1
      @broker.active_calls[@channel.id][the_session.id].should == the_session
    end

    it "send ringing notification" do
      queued_call = @channel.queued_calls.make

      @broker.should_receive(:call).with { |session| session.should_receive(:notify_status).with('ringing') }
      @broker.notify_call_queued @channel
    end

    it "close session if call fails" do
      queued_call = @channel.queued_calls.make
      the_session = nil

      @broker.should_receive(:call).with { |session| the_session = session }.and_raise Exception.new
      @broker.notify_call_queued @channel

      @broker.sessions.length.should == 0
      @broker.active_calls_count_for(@channel).should == 0
    end

    it "requeue call if pbx unavailable on call" do
      queued_call = @channel.queued_calls.make
      the_session = nil

      @broker.should_receive(:call).with { |session| the_session = session }.and_raise PbxUnavailableException.new
      @broker.notify_call_queued @channel

      assert_equal [queued_call.attributes.except('id', 'created_at', 'updated_at')], @channel.queued_calls.all.map{|x| x.attributes.except('id', 'created_at', 'updated_at')}.to_a

      queued_call = @channel.queued_calls.first
      queued_call.call_log.state.should == :queued

      @broker.sessions.length.should == 0
      @broker.active_calls_count_for(@channel).should == 0
    end

    it "requeue call if rejected and retries available by queue" do
      queue = @channel.account.call_queues.make :retries => '1,2,4'
      queued_call = @channel.queued_calls.make :call_queue => queue, :retries => 1
      the_session = nil

      @broker.should_receive(:call).with { |session| the_session = session }
      @broker.notify_call_queued @channel

      EM.should_receive(:fiber_sleep).with 2
      @broker.call_rejected the_session.id, :busy

      queued_call = QueuedCall.last
      (queued_call.not_before - (Time.now + 2.hour)).abs.should <= 2.seconds
    end

    it "do not requeue if all retries has been used" do
      queue = @channel.account.call_queues.make :retries => '1,2,4'
      queued_call = @channel.queued_calls.make :call_queue => queue, :retries => 3
      the_session = nil

      @broker.should_receive(:call).with { |session| the_session = session }
      @broker.notify_call_queued @channel

      EM.should_receive(:fiber_sleep).with 2
      @broker.call_rejected the_session.id, :busy

      QueuedCall.last.should be_nil
    end

    it "not call if limit reached" do
      @channel.limit = 1

      queued_call_1 = @channel.queued_calls.make
      queued_call_2 = @channel.queued_calls.make

      the_session = nil
      @broker.should_receive(:call).once.with { |session| the_session = session }
      @broker.notify_call_queued @channel

      @broker.notify_call_queued @channel
    end

    it "not call if scheduled for the future" do
      @channel.queued_calls.make :not_before => Time.now + 1.hour

      @broker.should_receive(:call).never
      @broker.notify_call_queued @channel
    end
  end

  context "finish session" do
    it "finish session successfully" do
      queued_call = @channel.queued_calls.make
      the_session = nil

      @broker.should_receive(:call).with { |session| the_session = session }
      @broker.notify_call_queued @channel

      @broker.finish_session_successfully the_session
      @broker.sessions.length.should == 0
      @broker.active_calls[@channel.id].length.should == 0
      the_session.call_log.state.should == :completed
    end

    it "finish session with error" do
      queued_call = @channel.queued_calls.make
      the_session = nil

      @broker.should_receive(:call).with { |session| the_session = session }
      @broker.notify_call_queued @channel

      @broker.finish_session_with_error the_session, 'An error'
      @broker.sessions.length.should == 0
      @broker.active_calls[@channel.id].length.should == 0
      assert_match /An error/, the_session.call_log.details
      the_session.call_log.state.should == :failed
    end

    it "send status callback on successfull" do
      queued_call = @channel.queued_calls.make
      the_session = nil

      @broker.should_receive(:call).with { |session| the_session = session }
      @broker.notify_call_queued @channel

      the_session.application.status_callback_url = 'http://foo'
      the_session.pbx = mock('pbx')
      the_session.should_receive(:notify_status).with('completed')
      @broker.finish_session_successfully the_session
    end

    it "send status callback on failure" do
      queued_call = @channel.queued_calls.make
      the_session = nil

      @broker.should_receive(:call).with { |session| the_session = session }
      @broker.notify_call_queued @channel

      the_session.application.status_callback_url = 'http://foo'
      the_session.pbx = mock('pbx')
      the_session.should_receive(:notify_status).with('failed')
      @broker.finish_session_with_error the_session, 'An error'
    end

    it "send status callback on failure with custom status" do
      queued_call = @channel.queued_calls.make
      the_session = nil

      @broker.should_receive(:call).with { |session| the_session = session }
      @broker.notify_call_queued @channel

      the_session.application.status_callback_url = 'http://foo'
      the_session.pbx = mock('pbx')
      the_session.should_receive(:notify_status).with('busy')
      @broker.finish_session_with_error the_session, 'An error', 'busy'
    end
  end

  context "reject call" do
    it "reject with busy status" do
      session = Session.new
      @broker.should_receive(:find_session).with(123).and_return(session)
      @broker.should_receive(:finish_session_with_error).with(session, 'Remote end is busy', 'busy')
      EM.should_receive(:fiber_sleep).with 2
      @broker.should_receive(:notify_call_queued)

      @broker.call_rejected 123, :busy
    end

    it "reject with no answer status" do
      session = Session.new
      @broker.should_receive(:find_session).with(123).and_return(session)
      @broker.should_receive(:finish_session_with_error).with(session, 'Remote end do not answer', 'no-answer')
      EM.should_receive(:fiber_sleep).with 2
      @broker.should_receive(:notify_call_queued)

      @broker.call_rejected 123, :no_answer
    end

    it "reject with unknown reason" do
      session = Session.new
      @broker.should_receive(:find_session).with(123).and_return(session)
      @broker.should_receive(:finish_session_with_error).with(session, 'Failed to establish the communication', 'failed')
      EM.should_receive(:fiber_sleep).with 2
      @broker.should_receive(:notify_call_queued)

      @broker.call_rejected 123, :failed
    end
  end

  context "accept call" do
    it "run when there is already a session" do
      @channel.application.flow = Compiler.make { Answer(); Hangup() }
      @channel.application.save!

      queued_call = @channel.queued_calls.make
      the_session = nil
      @broker.should_receive(:call).with { |session| the_session = session }
      @broker.notify_call_queued @channel

      pbx = stub 'pbx', :session_id => the_session.id
      pbx.should_receive :answer
      pbx.should_receive(:hangup).twice

      EM.should_receive(:fiber_sleep).with 2

      @broker.accept_call pbx

      the_session.call_log.address.should == queued_call.address
      the_session.call_log.state.should == :completed
      @broker.sessions.length.should == 0
      @broker.active_calls[@channel.id].length.should == 0
    end

    it "run when there is no session" do
      @channel.application.flow = Compiler.make { Answer(); Hangup() }
      @channel.application.save!

      pbx = stub 'pbx', :session_id => nil, :channel_id => @channel.id, :caller_id => '1234'
      pbx.should_receive :answer
      pbx.should_receive(:hangup).twice

      EM.should_receive(:fiber_sleep).with 2

      @broker.accept_call pbx

      call_logs = CallLog.all
      call_logs.length.should == 1
      call_log = call_logs.first

      call_log.address.should == '1234'
      call_log.state.should == :completed
      @broker.sessions.length.should == 0
      @broker.active_calls[@channel.id].length.should == 0
    end

    it "resume when there is a suspended session" do
      session = Session.new
      session.suspend
      @broker.sessions['id'] = session

      pbx = stub 'pbx', :session_id => 'id'

      session.should_receive(:resume)
      @broker.accept_call pbx
    end

    it "resume and close pbx connection" do
      @channel.application.flow = Compiler.make { Yield(); Hangup() }
      @channel.application.save!

      pbx = stub 'pbx', :session_id => nil, :channel_id => @channel.id, :caller_id => '1234'

      f = Fiber.new do
        @broker.accept_call pbx
      end

      # This will start the session and yield at the YieldCommand
      f.resume

      pbx.should_receive :close_connection
      session = @broker.sessions.values.first
      @broker.should_receive(:restart).with(session)
      @broker.redirect session.call_log.id, :flow => Compiler.make { Hangup() }

      # Resume the session and yields because it is suspended
      f.resume

      pbx2 = stub 'pbx2', :session_id => session.id, :channel_id => nil, :caller_id => '1234'
      pbx2.should_receive(:hangup).twice
      EM.should_receive(:fiber_sleep).with 2

      @broker.accept_call pbx2
    end

    it "send 'in-progress' status notification" do
      @channel.application.flow = Compiler.make { Answer() }
      @channel.application.save!

      class << @broker
        def find_or_create_session(pbx)
          session = super
          session.should_receive(:notify_status).with('in-progress')
          session.should_receive(:notify_status).with('completed')
          session
        end
      end

      pbx = stub 'pbx', :session_id => nil, :channel_id => @channel.id, :caller_id => '1234'
      pbx.should_receive :answer
      pbx.should_receive :hangup

      EM.should_receive(:fiber_sleep).with 2

      @broker.accept_call pbx
    end
  end
end

class Commands::YieldCommand < Command
  def run(session)
    Fiber.yield
    super
  end
end

