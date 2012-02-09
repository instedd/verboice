require 'spec_helper'

describe Asterisk::Broker do
  before(:each) do
    @broker = Asterisk::Broker.new
    $asterisk_client = mock('asterisk_client')
    @channel = Channel.make :kind => 'sip2sip'
  end

  context "call" do
    before(:each) do
      @session = Session.new :channel => @channel, :address => 'Foo'
    end

    it "call ok" do
      $asterisk_client.should_receive(:error?).and_return(false)
      $asterisk_client.should_receive(:originate).with({
        :channel => "SIP/verboice_#{@channel.id}-0/#{@session.address}",
        :application => 'AGI',
        :data => "agi://localhost:#{Asterisk::CallManager::Port},#{@session.id}",
        :async => true,
        :actionid => @session.id
      }).and_return(:response => 'OK')

      result = @broker.call @session
      result.should == nil
    end

    it "call fails on asterisk_client error" do
      $asterisk_client.should_receive(:error?).and_return(true)

      ex = assert_raise(PbxUnavailableException) { @broker.call @session }
      assert_match /not available/, ex.message
    end

    it "call fails on originate error" do
      $asterisk_client.should_receive(:error?).and_return(false)
      $asterisk_client.should_receive(:originate).with({
        :channel => "SIP/verboice_#{@channel.id}-0/#{@session.address}",
        :application => 'AGI',
        :data => "agi://localhost:#{Asterisk::CallManager::Port},#{@session.id}",
        :async => true,
        :actionid => @session.id
      }).and_return(:response => 'Error', :message => 'Oops')

      ex = assert_raise(RuntimeError) { @broker.call @session }
      ex.message.should == 'Oops'
    end
  end
end
