require 'spec_helper'

describe CallbackCommand do
  before(:each) do
    @session = Session.new
    @session.pbx = mock('pbx')
    @session.pbx.should_receive(:caller_id).and_return('999')
    @session.channel = mock('channel')
    @session.channel.should_receive(:name).and_return('foo')
    @default_body = {:From => '999', :Channel => 'foo'}
  end

  it "run with url as string" do
    url = 'http://www.example.com'

    @session.call_log = CallLog.make
    @session.should_receive(:log).with({
      :info => "Callback post #{url}",
      :trace => "Callback post #{url} with CallSid=#{@session.call_id}&Channel=foo&From=999"
    })
    @session.should_receive(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")
    @session.should_receive(:push_commands).with([:hangup])

    expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
      CallbackCommand.new(url).run @session
    end
  end

  it "run with url as option" do
    url = 'http://www.example.com'

    @session.call_log = CallLog.make
    @session.should_receive(:log).with({
      :info => "Callback post #{url}",
      :trace => "Callback post #{url} with CallSid=#{@session.call_id}&Channel=foo&From=999"
    })
    @session.should_receive(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")
    @session.should_receive(:push_commands).with([:hangup])

    expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
      CallbackCommand.new(:url => url).run @session
    end
  end

  it "run with url and get method" do
    url = 'http://www.example.com'

    @session.call_log = CallLog.make
    @session.should_receive(:log).with({
      :info => "Callback get #{url}",
      :trace => "Callback get #{url} with CallSid=#{@session.call_id}&Channel=foo&From=999"
    })
    @session.should_receive(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")
    @session.should_receive(:push_commands).with([:hangup])

    expect_em_http :get, url, :with => @default_body.merge(:CallSid => @session.call_id), :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
      CallbackCommand.new(:url => url, :method => :get).run @session
    end
  end

  it "run without url" do
    url = 'http://www.example.com'

    @session.application = Session.new :application => mock('application')
    @session.call_log = CallLog.make
    @session.should_receive(:callback_url).and_return(url)
    @session.should_receive(:log).with({
      :info => "Callback post #{url}",
      :trace => "Callback post #{url} with CallSid=#{@session.call_id}&Channel=foo&From=999"
    })
    @session.should_receive(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")
    @session.should_receive(:push_commands).with([:hangup])

    expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
      CallbackCommand.new.run @session
    end
  end

  it "run receives json in response" do
    url = 'http://www.example.com'

    @session.call_log = CallLog.make
    @session.should_receive(:log).with({
      :info => "Callback post #{url}",
      :trace => "Callback post #{url} with CallSid=#{@session.call_id}&Channel=foo&From=999"
    })
    @session.should_receive(:trace).with("Callback returned application/json: hangup();")
    @session.should_receive(:push_commands).with([:js => 'hangup();'])

    expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id)}, :and_return => 'hangup();', :content_type => 'application/json' do
      CallbackCommand.new(url).run @session
    end
  end

  it "run with custom parameters" do
    url = 'http://www.example.com'

    @session.call_log = CallLog.make
    @session[:digits] = '123'
    @session.should_receive(:log).with({
      :info => "Callback post #{url}",
      :trace => "Callback post #{url} with CallSid=#{@session.call_id}&Channel=foo&Digits=123&From=999"
    })
    @session.should_receive(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")
    @session.should_receive(:push_commands).with([:hangup])

    expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id, :Digits => '123')}, :and_return => '<Response><Hangup/></Response>', :content_type => 'application/xml' do
      CallbackCommand.new(:url => url, :params => {:Digits => :digits}).run @session
    end
  end
end
