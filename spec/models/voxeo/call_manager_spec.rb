require 'spec_helper'

describe Voxeo::CallManager do
  
  let(:channel_id) { 123 }
  let(:voxeo_session_id) { 'abcd1234' }
  let(:session_id) { 345 }
  let(:caller_id) { 678 }
  let(:builder) { double('builder') }
  let(:context) { double('context') }
  let(:call_manager) { Voxeo::CallManager.new channel_id, voxeo_session_id, {:session_id => session_id, :caller_id => caller_id, :context => context} }
  
  before(:each) do
    Builders::Vxml.should_receive(:new).and_return(builder)
  end
  
  it "should tell session id" do
    call_manager.session_id.should eq(session_id)
  end
  
  it "should tell voxeo session id" do
    call_manager.voxeo_session_id.should eq(voxeo_session_id)
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
    Rails.configuration.voxeo_configuration[:sounds_url] = "http://www.foo.com/bar"
    
    builder.should_receive(:play).with("http://www.foo.com/bar/sound.gsm").and_return(builder)
    call_manager.play(call_manager.sound_path_for("sound"))
  end
  
  it "should build xml for say" do
    builder.should_receive(:say).with("some text")
    call_manager.say("some text")
  end
  
  it "should build xml for pause" do
    builder.should_receive(:pause).with(13)
    call_manager.pause(13)
  end
  
  it "should tell sound path using rails public folder" do
    basename = "some/path/foo"
    call_manager.sound_path_for(basename).should eq(File.join(Rails.public_path, "sounds", "#{basename}.gsm"))
  end
  
  context "capture" do
    
    let(:options) { {:foo => "bar"} }
    
    before :each do
      Rails.configuration.voxeo_configuration[:http_url_options] = {:host => "serverhost.com", :port => 1234}
      Rails.configuration.voxeo_configuration[:sounds_url] = "http://www.foo.com/bar"
      expect_flush({:digits => "123"})
    end
    
    it "should build capture and callback xml" do
      builder.should_receive(:capture).with(options)
      builder.should_receive(:callback).with("http://serverhost.com:1234/")
      
      call_manager.capture(options).should eq("123")
    end
    
    it 'should use host from request when url options is undefined' do
      Rails.configuration.voxeo_configuration[:http_url_options] = {}
      builder.should_receive(:capture).with(options)
      context.should_receive(:headers).and_return({:Host => "hostfromrequest.com:5678"})
      builder.should_receive(:callback).with("http://hostfromrequest.com:5678/")
      
      call_manager.capture(options)
    end
    
    it 'should use sound url when options has play' do
      options = {:play => call_manager.sound_path_for('sound')}
      expected_options = {:play => 'http://www.foo.com/bar/sound.gsm'}
      builder.should_receive(:capture).with(expected_options)
      builder.should_receive(:callback).with("http://serverhost.com:1234/")
      
      call_manager.capture(options)
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
      
      EM.should_receive(:next_tick) do |&block|
        block.call
        true
      end
      
      call_manager.hangup
    end
    
    it 'should delete the fiber from the store' do
      EM.stub(:next_tick)
      store = double('store')
      store.should_receive(:delete_fiber_for).with(voxeo_session_id)
      Voxeo::FiberStore.should_receive(:instance).and_return(store)
      
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
    context = double('context', :params => response)
    builder.should_receive(:build).and_return("xml")
    Fiber.should_receive(:yield).with("xml").and_return(context)
  end

end