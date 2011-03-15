require 'test_helper'

class ApplicationTest < ActiveSupport::TestCase
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
end
