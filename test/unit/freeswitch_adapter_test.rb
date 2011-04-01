require 'test_helper'

class FreeswitchAdapterTest < ActiveSupport::TestCase
  setup do
    @context = mock('context')
    @adapter = Freeswitch::Adapter.new @context
  end

  test "application_id" do
    @context.stubs(:session => {:variable_verboice_application_id => :id})
    assert_equal :id, @adapter.application_id
  end

  test "call_log_id" do
    @context.stubs(:session => {:variable_verboice_call_log_id => :id})
    assert_equal :id, @adapter.call_log_id
  end

  [:answer, :hangup].each do |cmd|
    test cmd.to_s do
      @context.expects(cmd)
      @adapter.send cmd
    end
  end

  test "play" do
    @context.expects(:playback).with(:url)
    @adapter.play :url
  end

  context "capture" do
    should "capture one digit" do
      @context.expects(:read).with('silence_stream://1', :min => 1, :max => 1, :terminators => '', :timeout => 5000, :variable => 'last_capture').returns('1')
      value = @adapter.capture :min => 1, :max => 1, :finish_on_key => '', :timeout => 5
      assert_equal '1', value
    end

    should "capture non default options" do
      @context.expects(:read).with('silence_stream://1', :min => 2, :max => 5, :terminators => '*', :timeout => 3000, :variable => 'last_capture').returns('1')
      value = @adapter.capture :min => 2, :max => 5, :finish_on_key => '*', :timeout => 3
      assert_equal '1', value
    end

    should "capture while playing" do
      @context.expects(:read).with('some_file', :min => 2, :max => 5, :terminators => '*', :timeout => 3000, :variable => 'last_capture').returns('1')
      value = @adapter.capture :min => 2, :max => 5, :finish_on_key => '*', :timeout => 3, :play => 'some_file'
      assert_equal '1', value
    end
  end
end
