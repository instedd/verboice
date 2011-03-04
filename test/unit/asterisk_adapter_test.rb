require 'test_helper'

class AsteriskAdapterTest < ActiveSupport::TestCase
  setup do
    @context = mock('context')
    @adapter = AsteriskAdapter.new @context
  end

  test "answer" do
    @context.expects(:answer)
    @adapter.answer
  end
end
