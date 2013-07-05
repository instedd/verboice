# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

require 'spec_helper'

describe BaseBroker do
  Channel.all_leaf_subclasses.reject{|a_subclass| a_subclass == Channels::TemplateBasedSip}.each do |a_channel|
    it "shouldn't call if call limit is reached" do
      @broker = BaseBroker.new
      @broker.stub(:pbx_available? => true)
      @channel = a_channel.make

      @channel.limit = 1

      queued_call_1 = @channel.queued_calls.make
      queued_call_2 = @channel.queued_calls.make

      the_session = nil
      @broker.should_receive(:call).once.with { |session| the_session = session }
      @broker.notify_call_queued @channel

      @broker.notify_call_queued @channel
    end
  end

  it "shouldn't call if call limit is reached" do
    @broker = BaseBroker.new
    @broker.stub(:pbx_available? => true)
    @channel = Channels::TemplateBasedSip.make :limit => 1

    queued_call_1 = @channel.queued_calls.make
    queued_call_2 = @channel.queued_calls.make

    the_session = nil
    @broker.should_receive(:call).once.with { |session| the_session = session }
    @broker.notify_call_queued @channel

    @broker.notify_call_queued @channel
  end

  it "should call if call limit is reached but session is suspended" do
    @broker = BaseBroker.new
    @broker.stub(:pbx_available? => true)
    @channel = Channels::TemplateBasedSip.make

    queued_call_1 = @channel.queued_calls.make
    queued_call_2 = @channel.queued_calls.make

    the_session = nil
    @broker.should_receive(:call).twice.with { |session| the_session = session }

    @broker.notify_call_queued @channel

    the_session.suspend

    @broker.notify_call_queued @channel
  end

  Channel.all_leaf_subclasses.each do |a_channel|
    context "channel #{a_channel}" do
      before(:each) do
        @broker = BaseBroker.new
        @broker.stub(:pbx_available? => true)
        @channel = a_channel.make
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

        it "resumes a suspended session" do
          queued_call = @channel.queued_calls.make
          the_session = nil

          @broker.should_receive(:call).twice.with do |session|
            if the_session
              session.equal?(the_session)
            else
              the_session = session
            end
          end
          @broker.notify_call_queued @channel

          the_session.suspend

          queued_call = @channel.queued_calls.make session_id: the_session.id
          @broker.notify_call_queued @channel
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
          schedule = @channel.project.schedules.make :retries => '1,2,4', :time_to => (Time.now + 6.hour)
          queued_call = @channel.queued_calls.make :schedule => schedule, :retries => 1
          the_session = nil

          @broker.should_receive(:call).with { |session| the_session = session }
          @broker.notify_call_queued @channel

          EM.should_receive(:fiber_sleep).with 2
          @broker.call_rejected the_session.id, :busy

          queued_call = QueuedCall.last
          (queued_call.not_before - (Time.now + 2.hour)).abs.should <= 2.seconds
        end

        it "requeue call using time zone" do
          # It's 15.30 UTC, 12:30 ARG
          Timecop.freeze(Time.utc(2012,1,1,15,30))
          schedule = @channel.project.schedules.make :retries => '1', :time_from => (Time.now.utc - 3.hour), :time_to => (Time.now.utc + 6.hour)
          queued_call = @channel.queued_calls.make :schedule => schedule, :retries => 0, :time_zone => 'Buenos Aires'
          the_session = nil

          @broker.should_receive(:call).with { |session| the_session = session }
          @broker.notify_call_queued @channel

          EM.should_receive(:fiber_sleep).with 2
          @broker.call_rejected the_session.id, :busy

          queued_call = QueuedCall.last
          queued_call.not_before.should eq(Time.utc(2012,1,1,16,30))
          Timecop.return
        end

        it "do not requeue if all retries has been used" do
          schedule = @channel.project.schedules.make :retries => '1,2,4'
          queued_call = @channel.queued_calls.make :schedule => schedule, :retries => 3
          the_session = nil

          @broker.should_receive(:call).with { |session| the_session = session }
          @broker.notify_call_queued @channel

          EM.should_receive(:fiber_sleep).with 2
          @broker.call_rejected the_session.id, :busy

          QueuedCall.last.should be_nil
        end

        it "requeue call if rejected and the contact has more numbers to try" do
          contact = @channel.project.contacts.make
          second_address = contact.addresses.make
          contact.addresses.count.should eq(2)

          queued_call = @channel.queued_calls.make :address => contact.first_address, :call_log => @channel.call_logs.make
          the_session = nil

          @broker.should_receive(:call).with { |session| the_session = session }
          @broker.notify_call_queued @channel

          EM.should_receive(:fiber_sleep).with 2
          @broker.call_rejected the_session.id, :busy

          queued_call = QueuedCall.last
          (queued_call.not_before - (Time.now + 15.seconds)).abs.should <= 2.seconds
          queued_call.address.should eq(second_address.address)
          queued_call.retries.should eq(0)
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

        it "finish session successfully with status failed" do
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

          @broker.finish_session_with_error the_session, 'A foobar error'
          @broker.sessions.length.should == 0
          @broker.active_calls[@channel.id].length.should == 0
          assert_match /A foobar error/, the_session.call_log.entries.last.description
          the_session.call_log.state.should == :failed
        end

        it "send status callback on successfull" do
          queued_call = @channel.queued_calls.make
          the_session = nil

          @broker.should_receive(:call).with { |session| the_session = session }
          @broker.notify_call_queued @channel

          the_session.call_flow.project.status_callback_url = 'http://foo'
          the_session.pbx = mock('pbx')
          the_session.should_receive(:notify_status).with('completed')
          @broker.finish_session_successfully the_session
        end

        it "send status callback on failure" do
          queued_call = @channel.queued_calls.make
          the_session = nil

          @broker.should_receive(:call).with { |session| the_session = session }
          @broker.notify_call_queued @channel

          the_session.call_flow.project.status_callback_url = 'http://foo'
          the_session.pbx = mock('pbx')
          the_session.should_receive(:notify_status).with('failed')
          @broker.finish_session_with_error the_session, 'An error'
        end

        it "send status callback on failure with custom status" do
          queued_call = @channel.queued_calls.make
          the_session = nil

          @broker.should_receive(:call).with { |session| the_session = session }
          @broker.notify_call_queued @channel

          the_session.call_flow.project.status_callback_url = 'http://foo'
          the_session.pbx = mock('pbx')
          the_session.should_receive(:notify_status).with('busy')
          @broker.finish_session_with_error the_session, 'An error', 'busy'
        end

        it "should create a Trace logging that the call has ended" do
          @channel.call_flow.user_flow = [
            {
              'id' => 1,
              'root' => 1,
              'type' => 'play',
              'name' => 'Play number one',
              'resource' => {
                "guid" => TextLocalizedResource.make.guid
              }
            }
          ]
          @channel.call_flow.save!
          @channel.call_flow.reload

          queued_call = @channel.queued_calls.make
          the_session = nil

          @broker.should_receive(:call).with { |session| the_session = session }
          @broker.notify_call_queued @channel

          pbx = stub 'pbx', :session_id => the_session.id
          pbx.should_receive :session=
          pbx.should_receive :answer
          pbx.should_receive :hangup

          EM.should_receive(:fiber_sleep).with 2

          @broker.accept_call pbx

          Trace.all.size.should eq(2)
          Trace.first.result.should eq('Message played.')
          Trace.first.call_flow_id.should eq(@channel.call_flow.id)
          Trace.first.step_id.to_i.should eq(1)
          Trace.first.step_name.should eq('Play number one')

          Trace.last.result.should eq('User hung up.')
          Trace.last.call_flow_id.should eq(@channel.call_flow.id)
          Trace.last.step_id.to_i.should eq(1)
          Trace.last.step_name.should eq('')
        end
      end

      context "reject call" do
        it "reject with busy status" do
          session = Session.new call_log: CallLog.make
          @broker.should_receive(:find_session).with(123).and_return(session)
          @broker.should_receive(:finish_session_with_error).with(session, 'Remote end is busy', 'busy')
          EM.should_receive(:fiber_sleep).with 2
          @broker.should_receive(:notify_call_queued)

          @broker.call_rejected 123, :busy
        end

        it "reject with no answer status" do
          session = Session.new call_log: CallLog.make
          @broker.should_receive(:find_session).with(123).and_return(session)
          @broker.should_receive(:finish_session_with_error).with(session, 'Remote end do not answer', 'no-answer')
          EM.should_receive(:fiber_sleep).with 2
          @broker.should_receive(:notify_call_queued)

          @broker.call_rejected 123, :no_answer
        end

        it "reject with unknown reason" do
          session = Session.new call_log: CallLog.make
          @broker.should_receive(:find_session).with(123).and_return(session)
          @broker.should_receive(:finish_session_with_error).with(session, 'Failed to establish the communication', 'failed')
          EM.should_receive(:fiber_sleep).with 2
          @broker.should_receive(:notify_call_queued)

          @broker.call_rejected 123, :failed
        end
      end

      context "accept call" do
        it "run when there is already a session" do
          @channel.call_flow.flow = Compiler.make { Answer(); Hangup() }
          @channel.call_flow.save!

          queued_call = @channel.queued_calls.make
          the_session = nil
          @broker.should_receive(:call).with { |session| the_session = session }
          @broker.notify_call_queued @channel

          pbx = stub 'pbx', :session_id => the_session.id
          pbx.should_receive :session=
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
          @channel.call_flow.flow = Compiler.make { Answer(); Hangup() }
          @channel.call_flow.save!

          pbx = stub 'pbx', :session_id => nil, :channel_id => @channel.id, :caller_id => '1234'
          pbx.should_receive :session=
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
          pbx.should_receive :session=

          session.should_receive(:resume)
          @broker.accept_call pbx
        end

        it "resume and close pbx connection" do
          @channel.call_flow.flow = Compiler.make { Yield(); Hangup() }
          @channel.call_flow.save!

          pbx = stub 'pbx', :session_id => nil, :channel_id => @channel.id, :caller_id => '1234'
          pbx.should_receive :session=

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
          pbx2.should_receive(:session=)
          pbx2.should_receive(:hangup).twice
          EM.should_receive(:fiber_sleep).with 2

          @broker.accept_call pbx2
        end

        it "send 'in-progress' status notification" do
          @channel.call_flow.flow = Compiler.make { Answer() }
          @channel.call_flow.save!

          class << @broker
            def find_or_create_session(pbx)
              session = super
              session.should_receive(:notify_status).with('in-progress')
              session.should_receive(:notify_status).with('completed')
              session
            end
          end

          pbx = stub 'pbx', :session_id => nil, :channel_id => @channel.id, :caller_id => '1234'
          pbx.should_receive :session=
          pbx.should_receive :answer
          pbx.should_receive :hangup

          EM.should_receive(:fiber_sleep).with 2

          @broker.accept_call pbx
        end
      end
    end
  end
end

class Commands::YieldCommand < Command
  def run(session)
    Fiber.yield
    super
  end
end
