require 'spec_helper'

describe QueuedCall do
  it 'create new session with custom callback url' do
    qcall = QueuedCall.make :callback_url => 'http://foo.com'
    session = qcall.new_session
    session.callback_url.should == 'http://foo.com'
    session.commands.should == Compiler.make { Answer(); Callback('http://foo.com') }
  end

  it 'create new session with custom flow' do
    qcall = QueuedCall.make :flow => Compiler.make { Answer(); Hangup() }
    session = qcall.new_session
    session.commands.should == Compiler.make { Answer(); Hangup() }
  end

  it 'create new session with custom callback and custom status callback url' do
    qcall = QueuedCall.make :callback_url => 'http://callback', :status_callback_url => 'http://foo.com'
    session = qcall.new_session
    session.status_callback_url.should == 'http://foo.com'
  end
end
