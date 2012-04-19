require 'spec_helper'

describe Channel do
  # Because we have after_commit callbacks...
  self.use_transactional_fixtures = false

  after(:each) do
    BrokerClient.stub(:delete_channel)
    [Account, Channel, CallLog, CallQueue, QueuedCall].each &:destroy_all
  end

  context "validations" do
    before(:each) { Channel.make }

    it { should belong_to(:account) }
    it { should belong_to(:application) }

    it { should validate_presence_of(:account) }
    it { should validate_presence_of(:application) }
    it { should validate_presence_of(:name) }
    it { should validate_uniqueness_of(:name).scoped_to(:account_id) }
  end

  context "call" do
    let (:channel) { Channel.make }
    let (:queued_call) { channel.queued_calls.first }

    it "call ok" do
      BrokerClient.should_receive(:notify_call_queued).with(channel.id)

      call_log = channel.call 'foo'
      call_log.state.should == :queued
      call_log.address.should == 'foo'

      queued_calls = channel.queued_calls
      queued_calls.length.should == 1
      queued_calls[0].address.should == 'foo'
      queued_calls[0].call_log_id.should == call_log.id
    end

    it "call raises" do
      BrokerClient.should_receive(:notify_call_queued).with(channel.id).and_raise("Oh no!")

      call_log = channel.call 'foo'
      call_log.state.should == :failed
    end

    it "call and set direction outgoing" do
      BrokerClient.should_receive(:notify_call_queued)

      call_log = channel.call 'foo'
      call_log.direction.should == :outgoing
    end

    it "call with custom callback url" do
      BrokerClient.should_receive(:notify_call_queued)

      channel.call 'foo', :callback_url => 'bar'
      queued_call.callback_url.should == 'bar'
    end

    it "call with custom flow" do
      BrokerClient.should_receive(:notify_call_queued)
      channel.call 'foo', :flow => Compiler.make { Answer(); Hangup() }
      queued_call.flow.should == Compiler.make { Answer(); Hangup() }
    end

    it "call with custom status callback url" do
      BrokerClient.should_receive(:notify_call_queued)

      channel.call 'foo', :status_callback_url => 'bar'
      queued_call.status_callback_url.should == 'bar'
    end

    it "notify with time when scheduling delayed call" do
      time = Time.now.utc + 1.hour
      BrokerClient.should_receive(:notify_call_queued).with(channel.id, time)
      channel.call 'foo', :not_before => time
    end

    it "obey queue lower time bound" do
      queue = channel.account.call_queues.make :time_from => '10:00', :time_to => '12:00'
      BrokerClient.should_receive(:notify_call_queued)
      channel.call 'foo', :not_before => '2012-12-20T08:00:00', :queue => queue.name
      queued_call.not_before.should == '2012-12-20T10:00:00'
    end

    it "obey queue upper time bound" do
      queue = channel.account.call_queues.make :time_from => '10:00', :time_to => '12:00'
      BrokerClient.should_receive(:notify_call_queued)
      channel.call 'foo', :not_before => '2012-12-20T13:00:00', :queue => queue.name
      queued_call.not_before.should == '2012-12-21T10:00:00'
    end
  end

  it "call BrokerClient.create_channel on create" do
    channel = Channel.make_unsaved
    BrokerClient.should_receive(:create_channel).with do |channel_id|
      channel_id == channel.id
    end
    channel.save!
  end

  it "call BrokerClient.delete_channel and BrokerClient.create_channel on update" do
    BrokerClient.should_receive(:create_channel)
    channel = Channel.make

    BrokerClient.should_receive(:delete_channel).with(channel.id).ordered
    BrokerClient.should_receive(:create_channel).with(channel.id).ordered

    channel.save!
  end

  it "call BrokerClient.delete_channel on destroy" do
    channel = Channel.make
    BrokerClient.should_receive(:delete_channel).with(channel.id)
    channel.destroy
  end

  context 'host and port' do
    before(:each) do
      @channel_with_host_and_port = Channel.new :config => { 'host_and_port' => 'host:1234' }
      @channel_without_host_and_port = Channel.new
    end

    it "return true for host_and_port?" do
      @channel_with_host_and_port.host_and_port?.should_not be_nil
    end

    it "return false for host_and_port?" do
      @channel_without_host_and_port.host_and_port?.should be_false
    end

    it "return host_and_port" do
      @channel_with_host_and_port.host_and_port.should eq(['host', '1234'])
    end
  end

  it "register? and_return true" do
    channel = Channel.new :config => { 'register' => '1' }
    channel.register?.should_not be_nil
  end

  it "register? and_return false" do
    channel = Channel.new :config => { 'register' => '0' }
    channel.register?.should be_false
  end

  context "poll call" do
    it "return nil if no queued calls" do
      channel = Channel.make
      channel.poll_call.should == nil
    end

    it "return queued call and destroy it" do
      channel = Channel.make
      queued_call = channel.queued_calls.make

      channel.poll_call.should == queued_call
      QueuedCall.count.should == 0
    end

    it "not return scheduled calls in the future" do
      channel = Channel.make
      channel.queued_calls.make :not_before => Time.now + 1.hour

      channel.poll_call.should == nil
    end
  end

  it "create new session without a call log" do
    channel = Channel.make
    session = channel.new_session
    session.call_log.account.should == channel.account
    session.call_log.application.should == channel.application
    session.call_log.channel.should == channel
    session.call_log.direction.should == :incoming
    session.call_log.state.should == :active
  end
  
  it "should assign a guid" do
    channel = Channel.make_unsaved
    channel.guid.should be_nil
    channel.save!
    channel.guid.should_not be_nil
  end
end
