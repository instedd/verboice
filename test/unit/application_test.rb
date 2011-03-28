require 'test_helper'

class ApplicationTest < ActiveSupport::TestCase
  test "sets name to callback url if name is empty" do
    app = Application.make :name => nil, :callback_url => 'foo'
    assert_equal app.callback_url, app.name
  end

  test "keeps name if name set" do
    app = Application.make :name => 'bar', :callback_url => 'foo'
    assert_equal 'bar', app.name
  end

  test "saves flow in json" do
    app = Application.make_unsaved
    app.flow = [:play => 'foo']
    app.save!

    app.reload
    assert_equal [:play => 'foo'], app.flow
  end

  test "commands is flow when present" do
    app = Application.make_unsaved
    app.flow = [:answer]
    assert_equal app.flow, app.commands
  end

  test "commands when callback url is present" do
    app = Application.make_unsaved
    app.callback_url = 'http://example.com'
    assert_equal [:answer, {:callback => app.callback_url}], app.commands
  end

  test "call ok" do
    app = Application.make
    app.expects(:with_pbx_interface).yields(client = mock('client'))

    the_call_log_id = nil

    client.expects(:call).with do |address, app_id, call_log_id|
      the_call_log_id = call_log_id
      address == 'foo' && app_id == app.id
    end

    call_log = app.call 'foo'
    assert_equal the_call_log_id, call_log.id
    assert_equal :active, call_log.state
  end

  test "call raises" do
    app = Application.make
    app.expects(:with_pbx_interface).yields(client = mock('client'))

    the_call_log_id = nil

    client.expects(:call).with do |address, app_id, call_log_id|
      the_call_log_id = call_log_id
      address == 'foo' && app_id == app.id
    end.raises("Oh no!")

    call_log = app.call 'foo'
    assert_equal the_call_log_id, call_log.id
    assert_equal :failed, call_log.state
  end
end
