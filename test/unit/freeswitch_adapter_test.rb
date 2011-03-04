require 'test_helper'

class FreeswitchAdapterTest < ActiveSupport::TestCase
  setup do
    @context = mock('context')
    @adapter = FreeswitchAdapter.new @context
  end

  test "answer" do
    @context.expects(:answer)
    @adapter.answer
  end
end
