require 'test_helper'

class ScriptTest < ActiveSupport::TestCase
  test "without args" do
    s = Script.new { answer }
    assert_equal [:answer], s
  end

  test "with args" do
    s = Script.new { play 'foo' }
    assert_equal [:play => 'foo'], s
  end

  test "puts" do
    s = Script.new { puts 'lala' }
    assert_equal [:puts => 'lala'], s
  end
end
