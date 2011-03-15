require 'test_helper'

class CallbackCommandTest < ActiveSupport::TestCase

  test "run" do
    url = 'http://www.example.com'
    session = Session.new
    session[:last_capture] = '123'

    http = mock('http')
    EventMachine::HttpRequest.expects(:new).with(url).returns(http)

    http2 = mock('http2')
    http.expects(:post).with(:body => "CallSid=#{session.id}&Digits=123").returns(http2)

    http2.expects(:response).returns('<Response><Hangup/></Response>')
    http2.expects(:callback).yields
    http2.expects(:errback)

    session.expects(:push_commands).with([:hangup])

    command = CallbackCommand.new url
    command.run session
  end

end