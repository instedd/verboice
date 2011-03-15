require 'test_helper'

class CaptureCommandTest < ActiveSupport::TestCase
  setup do
    @defaults = {:min => 1, :max => 1, :finish_on_key => '#', :timeout => 5}
    @session = Session.new :pbx => mock('pbx')
  end

  test "capture one key" do
    @session.pbx.expects(:capture).with(@defaults).returns(:digit)

    CaptureCommand.new.run @session

    assert_equal :digit, @session[:last_capture]
  end

  test "capture at least two keys" do
    @session.pbx.expects(:capture).with(@defaults.merge :min => 2, :max => Float::INFINITY).returns(:digit)

    CaptureCommand.new(:min => 2).run @session

    assert_equal :digit, @session[:last_capture]
  end

  test "capture at most three keys" do
    @session.pbx.expects(:capture).with(@defaults.merge :max => 3).returns(:digit)

    CaptureCommand.new(:max => 3).run @session

    assert_equal :digit, @session[:last_capture]
  end

  test "capture exactly four keys" do
    @session.pbx.expects(:capture).with(@defaults.merge :min => 4, :max => 4).returns(:digit)

    CaptureCommand.new(:min => 4, :max => 4).run @session

    assert_equal :digit, @session[:last_capture]
  end

  test "capture with timeout" do
    @session.pbx.expects(:capture).with(@defaults.merge :timeout => 1).returns(:digit)

    CaptureCommand.new(:timeout => 1).run @session

    assert_equal :digit, @session[:last_capture]
  end

  test "capture with finish on key" do
    @session.pbx.expects(:capture).with(@defaults.merge :finish_on_key => '*').returns(:digit)

    CaptureCommand.new(:finish_on_key => '*').run @session

    assert_equal :digit, @session[:last_capture]
  end

  test "capture with play" do
    @session.pbx.expects(:capture).with(@defaults.merge :play => :target_path).returns(:digit)

    play = mock('play')
    play.expects(:download).with(@session).returns(:target_path)
    PlayCommand.expects(:new).with(:url).returns(play)

    CaptureCommand.new(:play => :url).run @session

    assert_equal :digit, @session[:last_capture]
  end
end
