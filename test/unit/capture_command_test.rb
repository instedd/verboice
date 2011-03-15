require 'test_helper'

class CaptureCommandTest < ActiveSupport::TestCase
  setup do
    @defaults = {:min => 1, :max => 1, :finish_on_key => '#', :timeout => 5}
  end

  test "capture one key" do
    @context = mock('context')
    @context.expects(:capture).with(@defaults).returns(:digit)
    @context.expects(:last_capture=).with(:digit)

    CaptureCommand.new.run @context
  end

  test "capture at least two keys" do
    @context = mock('context')
    @context.expects(:capture).with(@defaults.merge :min => 2, :max => Float::INFINITY).returns(:digit)
    @context.expects(:last_capture=).with(:digit)

    CaptureCommand.new(:min => 2).run @context
  end

  test "capture at most three keys" do
    @context = mock('context')
    @context.expects(:capture).with(@defaults.merge :max => 3).returns(:digit)
    @context.expects(:last_capture=).with(:digit)

    CaptureCommand.new(:max => 3).run @context
  end

  test "capture exactly four keys" do
    @context = mock('context')
    @context.expects(:capture).with(@defaults.merge :min => 4, :max => 4).returns(:digit)
    @context.expects(:last_capture=).with(:digit)

    CaptureCommand.new(:min => 4, :max => 4).run @context
  end

  test "capture with timeout" do
    @context = mock('context')
    @context.expects(:capture).with(@defaults.merge :timeout => 1).returns(:digit)
    @context.expects(:last_capture=).with(:digit)

    CaptureCommand.new(:timeout => 1).run @context
  end

  test "capture with finish on key" do
    @context = mock('context')
    @context.expects(:capture).with(@defaults.merge :finish_on_key => '*').returns(:digit)
    @context.expects(:last_capture=).with(:digit)

    CaptureCommand.new(:finish_on_key => '*').run @context
  end

  test "capture with play" do
    @context = mock('context')
    @context.expects(:capture).with(@defaults.merge :play => :target_path).returns(:digit)
    @context.expects(:last_capture=).with(:digit)

    play = mock('play')
    play.expects(:download).with(@context).returns(:target_path)
    PlayCommand.expects(:new).with(:url).returns(play)

    CaptureCommand.new(:play => :url).run @context
  end
end
