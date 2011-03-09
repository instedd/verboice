require 'test_helper'

class CaptureCommandTest < ActiveSupport::TestCase
  test "capture one key" do
    @context = mock('context')
    @context.expects(:capture).with(:min => 1, :max => 1, :finish_on_key => '#', :timeout => 5).returns(:digit)
    @context.expects(:set_last_capture).with(:digit)

    CaptureCommand.new.run @context
  end

  test "capture at least two keys" do
    @context = mock('context')
    @context.expects(:capture).with(:min => 2, :max => Float::INFINITY, :finish_on_key => '#', :timeout => 5).returns(:digit)
    @context.expects(:set_last_capture).with(:digit)

    CaptureCommand.new(:min => 2).run @context
  end

  test "capture at most three keys" do
    @context = mock('context')
    @context.expects(:capture).with(:min => 1, :max => 3, :finish_on_key => '#', :timeout => 5).returns(:digit)
    @context.expects(:set_last_capture).with(:digit)

    CaptureCommand.new(:max => 3).run @context
  end

  test "capture exactly four keys" do
    @context = mock('context')
    @context.expects(:capture).with(:min => 4, :max => 4, :finish_on_key => '#', :timeout => 5).returns(:digit)
    @context.expects(:set_last_capture).with(:digit)

    CaptureCommand.new(:min => 4, :max => 4).run @context
  end

  test "capture with timeout" do
    @context = mock('context')
    @context.expects(:capture).with(:min => 1, :max => 1, :finish_on_key => '#', :timeout => 1).returns(:digit)
    @context.expects(:set_last_capture).with(:digit)

    CaptureCommand.new(:timeout => 1).run @context
  end

  test "capture with finish on key" do
    @context = mock('context')
    @context.expects(:capture).with(:min => 1, :max => 1, :finish_on_key => '*', :timeout => 5).returns(:digit)
    @context.expects(:set_last_capture).with(:digit)

    CaptureCommand.new(:finish_on_key => '*').run @context
  end
end
