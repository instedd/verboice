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

  {'48' => '0',
   '49' => '1',
   '57' => '9',
   '35' => '#',
   '42' => '*',
   '0' => nil}.each do |result, digit|
    test "capture one digit #{digit}" do
      @context.expects(:wait_for_digit).with(5 * 1000).returns(stub('line', :result => result))
      value = @adapter.capture :min => 1, :max => 1, :finish_on_key => '', :timeout => 5
      assert_equal digit, value
    end
  end

  test "capture two digits" do
    seq = sequence('digits')

    @context.expects(:wait_for_digit).with(5 * 1000).returns(stub('line1', :result => '52')).in_sequence(seq)
    @context.expects(:wait_for_digit).with(5 * 1000).returns(stub('line2', :result => '50')).in_sequence(seq)
    value = @adapter.capture :min => 2, :max => 2, :finish_on_key => '#', :timeout => 5
    assert_equal '42', value
  end

  test "capture three digits timeout" do
    seq = sequence('digits')

    @context.expects(:wait_for_digit).with(5 * 1000).returns(stub('line1', :result => '52')).in_sequence(seq)
    @context.expects(:wait_for_digit).with(5 * 1000).returns(stub('line2', :result => '50')).in_sequence(seq)
    @context.expects(:wait_for_digit).with(5 * 1000).returns(stub('line2', :result => '0')).in_sequence(seq)
    value = @adapter.capture :min => 3, :max => 3, :finish_on_key => '#', :timeout => 5
    assert_equal nil, value
  end

  test "capture digits finishes on key" do
    seq = sequence('digits')

    @context.expects(:wait_for_digit).with(5 * 1000).returns(stub('line1', :result => '52')).in_sequence(seq)
    @context.expects(:wait_for_digit).with(5 * 1000).returns(stub('line2', :result => '50')).in_sequence(seq)
    @context.expects(:wait_for_digit).with(5 * 1000).returns(stub('line2', :result => '35')).in_sequence(seq)
    value = @adapter.capture :min => 1, :max => 5, :finish_on_key => '#', :timeout => 5
    assert_equal '42', value
  end
end
