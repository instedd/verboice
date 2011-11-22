require 'test_helper'

class CallbackCommandTest < ActiveSupport::TestCase
  setup do
    @session = Session.new
    @session.pbx = mock('pbx')
    @session.pbx.expects(:caller_id).returns('999')
    @session.channel = mock('channel')
    @session.channel.expects(:name).returns('foo')
    @default_body = {:From => '999', :Channel => 'foo'}
  end

  test "run with url as string" do
    url = 'http://www.example.com'

    @session.call_log = CallLog.make
    @session[:digits] = '123'
    @session.expects(:log).with({
      :info => "Callback post #{url}",
      :trace => "Callback post #{url} with CallSid=#{@session.call_id}&Channel=foo&Digits=123&From=999"
    })
    @session.expects(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")
    @session.expects(:push_commands).with([:hangup])

    expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id, :Digits => '123')}, :returns => '<Response><Hangup/></Response>', :content_type => 'application/xml'

    CallbackCommand.new(url).run @session
  end

  test "run with url as option" do
    url = 'http://www.example.com'

    @session.call_log = CallLog.make
    @session[:digits] = '123'
    @session.expects(:log).with({
      :info => "Callback post #{url}",
      :trace => "Callback post #{url} with CallSid=#{@session.call_id}&Channel=foo&Digits=123&From=999"
    })
    @session.expects(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")
    @session.expects(:push_commands).with([:hangup])

    expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id, :Digits => '123')}, :returns => '<Response><Hangup/></Response>', :content_type => 'application/xml'

    CallbackCommand.new(:url => url).run @session
  end

  test "run with url and get method" do
    url = 'http://www.example.com'

    @session.call_log = CallLog.make
    @session[:digits] = '123'
    @session.expects(:log).with({
      :info => "Callback get #{url}",
      :trace => "Callback get #{url} with CallSid=#{@session.call_id}&Channel=foo&Digits=123&From=999"
    })
    @session.expects(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")
    @session.expects(:push_commands).with([:hangup])

    expect_em_http :get, url, :with => @default_body.merge(:CallSid => @session.call_id, :Digits => '123'), :returns => '<Response><Hangup/></Response>', :content_type => 'application/xml'

    CallbackCommand.new(:url => url, :method => :get).run @session
  end

  test "run without url" do
    url = 'http://www.example.com'

    @session.application = Session.new :application => mock('application')
    @session.call_log = CallLog.make
    @session.expects(:callback_url).returns(url)
    @session[:digits] = '123'
    @session.expects(:log).with({
      :info => "Callback post #{url}",
      :trace => "Callback post #{url} with CallSid=#{@session.call_id}&Channel=foo&Digits=123&From=999"
    })
    @session.expects(:trace).with("Callback returned application/xml: <Response><Hangup/></Response>")
    @session.expects(:push_commands).with([:hangup])

    expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id, :Digits => '123')}, :returns => '<Response><Hangup/></Response>', :content_type => 'application/xml'

    CallbackCommand.new.run @session
  end

  test "run receives json in response" do
    url = 'http://www.example.com'

    @session.call_log = CallLog.make
    @session[:digits] = '123'
    @session.expects(:log).with({
      :info => "Callback post #{url}",
      :trace => "Callback post #{url} with CallSid=#{@session.call_id}&Channel=foo&Digits=123&From=999"
    })
    @session.expects(:trace).with("Callback returned application/json: hangup();")
    @session.expects(:push_commands).with([:js => 'hangup();'])

    expect_em_http :post, url, :with => {:body => @default_body.merge(:CallSid => @session.call_id, :Digits => '123')}, :returns => 'hangup();', :content_type => 'application/json'

    CallbackCommand.new(url).run @session
  end

end
