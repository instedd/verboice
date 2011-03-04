require 'test_helper'

class FreeswitchAdapterTest < ActiveSupport::TestCase
  setup do
    @context = mock('context')
    @adapter = FreeswitchAdapter.new @context
  end

  [:answer, :hangup].each do |cmd|
    test cmd.to_s do
      @context.expects(cmd)
      @adapter.send cmd
    end
  end
end
