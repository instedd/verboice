require 'test_helper'

class AsteriskAdapterTest < ActiveSupport::TestCase
  setup do
    @context = mock('context')
    @adapter = AsteriskAdapter.new @context
  end

  test 'answer' do
    @context.expects :answer
    @adapter.send :answer
  end

  test 'hangup' do
    @context.expects :hangup
    @context.expects :close_connection
    @adapter.send :hangup
  end

  test "play" do
    path = @adapter.sound_path_for 'something'

    @context.expects(:stream_file).with('verbo/something', nil)
    @adapter.play path
  end
end
