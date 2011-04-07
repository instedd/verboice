require 'test_helper'

class CaptureCommandTest < ActiveSupport::TestCase
  setup do
    @defaults = {:min => 1, :max => 1, :finish_on_key => '#', :timeout => 5}
    @session = Session.new :pbx => mock('pbx')
    @digit = :digit
  end

  def expect_capture(options = {})
    options = @defaults.merge(options)

    @session.expects(:log).with(:info => "Waiting user input", :trace => "Waiting user input: #{options.to_pretty_s}")
    @session.pbx.expects(:capture).with(options).returns(@digit)
    if @digit
      @session.expects(:info).with("User pressed: #{@digit}")
    else
      @session.expects(:info).with("User didn't press anything")
    end
  end

  test "capture one key" do
    expect_capture

    CaptureCommand.new.run @session

    assert_equal :digit, @session[:capture]
  end

  test "capture one key timesout" do
    @digit = nil
    expect_capture

    CaptureCommand.new.run @session

    assert_nil @session[:capture]
  end

  test "capture at least two keys" do
    expect_capture :min => 2, :max => Float::INFINITY

    CaptureCommand.new(:min => 2).run @session

    assert_equal @digit, @session[:capture]
  end

  test "capture at most three keys" do
    expect_capture :max => 3

    CaptureCommand.new(:max => 3).run @session

    assert_equal @digit, @session[:capture]
  end

  test "capture exactly four keys" do
    expect_capture :min => 4, :max => 4

    CaptureCommand.new(:min => 4, :max => 4).run @session

    assert_equal @digit, @session[:capture]
  end

  test "capture with timeout" do
    expect_capture :timeout => 1

    CaptureCommand.new(:timeout => 1).run @session

    assert_equal @digit, @session[:capture]
  end

  test "capture with finish on key" do
    expect_capture :finish_on_key => '*'

    CaptureCommand.new(:finish_on_key => '*').run @session

    assert_equal @digit, @session[:capture]
  end

  test "capture with play empty" do
    expect_capture :finish_on_key => '*'

    CaptureCommand.new(:finish_on_key => '*', :play => ' ').run @session

    assert_equal @digit, @session[:capture]
  end

  test "capture with play" do
    @session.expects(:log).with(:info => "Waiting user input", :trace => "Waiting user input: #{@defaults.merge(:play => :url).to_pretty_s}")
    @session.pbx.expects(:capture).with(@defaults.merge(:play => :target_path)).returns(@digit)
    @session.expects(:info).with("User pressed: #{@digit}")

    play = mock('play')
    play.expects(:download).with(@session).returns(:target_path)
    PlayCommand.expects(:new).with(:url).returns(play)

    CaptureCommand.new(:play => :url).run @session

    assert_equal @digit, @session[:capture]
  end
end
