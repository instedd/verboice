require 'spec_helper'

describe Voxeo::CallManager do
  
  let(:channel_id) { 123 }
  let(:session_id) { 345 }
  let(:caller_id) { 678 }
  let(:builder) { double('builder') }
  let(:call_manager) { Voxeo::CallManager.new channel_id, session_id, caller_id }
  
  before(:each) do
    Builders::Vxml.should_receive(:new).and_return(builder)
  end
  
  it "should tell session id" do
    call_manager.session_id.should eq(session_id)
  end
  
  it "should tell channel id" do
    call_manager.channel_id.should eq(channel_id)
  end
  
  it "should tell called id" do
    call_manager.caller_id.should eq(caller_id)
  end
  
  it "defaults answering machine to false" do
    call_manager.is_answering_machine?.should be_false
  end
  
  it "should build xml for play" do
    builder.should_receive(:play).with("foo filename").and_return(builder)
    call_manager.play("foo filename")
  end
  
  it "should build xml for say" do
    builder.should_receive(:say).with("some text")
    call_manager.say("some text")
  end
  
  it "should build xml for pause" do
    builder.should_receive(:pause).with(13)
    call_manager.pause(13)
  end
  
  context "capture" do
    
    let(:options) { {:foo => "bar"} }
    
    it "should build capture and callback xml" do
      builder.should_receive(:capture).with(options)
      builder.should_receive(:callback).with("http://staging.instedd.org:7000/")
      expect_flush({:digits => "123"})
      
      call_manager.capture(options).should eq("123")
    end
    
  end
  
  context "hangup" do
    
    let(:fiber) { double('fiber') }
    
    before(:each) do
      builder.should_receive(:hangup)
      expect_flush
    end
    
    it "should build hangup xml" do
      EM.stub(:next_tick)
      
      call_manager.hangup
    end
    
    it "should resume fiber" do
      Fiber.should_receive(:current).and_return(fiber)
      fiber.should_receive(:resume)
      
      call_manager.hangup
    end
    
  end
  
  context "after hangup" do
    
    before(:each) do
      builder.should_receive(:hangup)
      EM.stub(:next_tick)
      expect_flush
      call_manager.hangup
    end
    
    it "should not play" do
      builder.should_not_receive(:play)
      call_manager.play "foo"
    end
    
    it "should not capture" do
      builder.should_not_receive(:capture)
      call_manager.capture({})
    end
    
    it "should not say" do
      builder.should_not_receive(:say)
      call_manager.say("foo")
    end
    
    it "should not hangup" do
      builder.should_not_receive(:hangup)
      call_manager.hangup
    end
    
    it "should not pause" do
      builder.should_not_receive(:pause)
      call_manager.pause(5)
    end
    
  end
  
  def expect_flush(response = {})
    builder.should_receive(:build).and_return("xml")
    Fiber.should_receive(:yield).with("xml").and_return(response)
  end

end