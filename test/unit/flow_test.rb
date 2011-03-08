require 'test_helper'

class FlowTest < ActiveSupport::TestCase
  setup do
    @context = mock('context')
    @flow = Flow.new @context
  end

  test "run no args command" do
    @context.expects(:foo)
    @flow.run { no_args }
  end

  test "run args command" do
    @context.expects(:bar).with(1)
    @flow.run { args :n => 1 }
  end
end

class NoArgsCommand
  def run(context)
    context.foo
  end
end

class ArgsCommand
  def initialize(options)
    @n = options[:n]
  end

  def run(context)
    context.bar @n
  end
end
