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
    pbx = mock('pbx')
    pbx.expects(:answer)

    app = Application.make_unsaved
    app.flow = [:answer]
    app.run pbx
  end

end
