require 'test_helper'

class QueuedCallTest < ActiveSupport::TestCase

  test 'create new session with custom callback url' do
    qcall = QueuedCall.make :callback_url => 'foo'
    session = qcall.new_session
    assert_equal [:answer, {:callback => 'foo'}], session.commands
  end

  test 'create new session with custom flow' do
    qcall = QueuedCall.make :flow => [:answer, :hangup]
    session = qcall.new_session
    assert_equal [:answer, :hangup], session.commands
  end

end