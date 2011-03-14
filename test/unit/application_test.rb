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
end
