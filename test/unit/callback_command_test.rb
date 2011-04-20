require 'test_helper'

class CallbackCommandTest < ActiveSupport::TestCase
  test "run with url as string" do
    url = 'http://www.example.com'

    session = Session.new
    session[:digits] = '123'
    session.expects(:log).with({
      :info => "Callback post #{url}",
      :trace => "Callback post #{url} with CallSid=#{session.id}&Digits=123"
    })
    session.expects(:trace).with("Callback returned: <Response><Hangup/></Response>")
    session.expects(:push_commands).with([:hangup])

    expect_em_http :post, url, :with => {:body => {:CallSid => session.id, :Digits => '123'}}, :returns => '<Response><Hangup/></Response>'

    CallbackCommand.new(url).run session
  end

  test "run with url as option" do
    url = 'http://www.example.com'

    session = Session.new
    session[:digits] = '123'
    session.expects(:log).with({
      :info => "Callback post #{url}",
      :trace => "Callback post #{url} with CallSid=#{session.id}&Digits=123"
    })
    session.expects(:trace).with("Callback returned: <Response><Hangup/></Response>")
    session.expects(:push_commands).with([:hangup])

    expect_em_http :post, url, :with => {:body => {:CallSid => session.id, :Digits => '123'}}, :returns => '<Response><Hangup/></Response>'

    CallbackCommand.new(:url => url).run session
  end

  test "run with url and get method" do
    url = 'http://www.example.com'

    session = Session.new
    session[:digits] = '123'
    session.expects(:log).with({
      :info => "Callback get #{url}",
      :trace => "Callback get #{url} with CallSid=#{session.id}&Digits=123"
    })
    session.expects(:trace).with("Callback returned: <Response><Hangup/></Response>")
    session.expects(:push_commands).with([:hangup])

    expect_em_http :get, url, :with => {:CallSid => session.id, :Digits => '123'}, :returns => '<Response><Hangup/></Response>'

    CallbackCommand.new(:url => url, :method => :get).run session
  end

  test "run without url" do
    url = 'http://www.example.com'

    session = Session.new :application => mock('application')
    session.expects(:callback_url).returns(url)
    session[:digits] = '123'
    session.expects(:log).with({
      :info => "Callback post #{url}",
      :trace => "Callback post #{url} with CallSid=#{session.id}&Digits=123"
    })
    session.expects(:trace).with("Callback returned: <Response><Hangup/></Response>")
    session.expects(:push_commands).with([:hangup])

    expect_em_http :post, url, :with => {:body => {:CallSid => session.id, :Digits => '123'}}, :returns => '<Response><Hangup/></Response>'

    CallbackCommand.new.run session
  end

end
