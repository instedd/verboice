require 'test_helper'

class QueuedCallTest < ActiveSupport::TestCase

  test 'create new session with custom callback url' do
    qcall = QueuedCall.make :callback_url => 'http://foo.com'
    session = qcall.new_session
    assert_equal 'http://foo.com', session.callback_url
    assert_equal [:answer, {:callback => 'http://foo.com'}], session.commands
  end

  test 'create new session with custom flow' do
    qcall = QueuedCall.make :flow => [:answer, :hangup]
    session = qcall.new_session
    assert_equal [:answer, :hangup], session.commands
  end

  test 'create new session with custom callback and custom status callback url' do
    qcall = QueuedCall.make :callback_url => 'http://callback', :status_callback_url => 'http://foo.com'
    session = qcall.new_session
    assert_equal 'http://foo.com', session.status_callback_url
  end

end