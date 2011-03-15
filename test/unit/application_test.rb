require 'test_helper'

class ApplicationTest < ActiveSupport::TestCase
  test "saves flow in json" do
    app = Application.make_unsaved
    app.flow = [:play => 'foo']
    app.save!

    app.reload
    assert_equal [:play => 'foo'], app.flow
  end

  test "run with flow" do
    context = mock('context')
    context.expects(:answer)

    app = Application.make_unsaved
    app.flow = [:answer]
    app.run context
  end

  test "run with callback url" do
    url = 'http://www.example.com'

    http = mock('http')
    EventMachine::HttpRequest.expects(:new).with(url).returns(http)

    http2 = mock('http2')
    http.expects(:post).with{|options| options[:body] =~ /^CallSid=.*?&Digits=123/}.returns(http2)

    http2.expects(:response).returns('<Response><Hangup/></Response>')
    http2.expects(:callback).yields
    http2.expects(:errback)

    context = mock('context')
    context.expects(:callback_url=).with(url)
    context.expects(:last_capture).returns('1234')
    context.expects(:answer)
    context.expects(:hangup)

    app = Application.make_unsaved
    app.callback_url = url

    app.run context
  end
end
