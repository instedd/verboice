require 'test_helper'

class ScriptTest < ActiveSupport::TestCase
  test "without args" do
    s = script { answer }
    assert_equal [:answer], s
  end

  test "with args" do
    s = script { play 'foo' }
    assert_equal [:play => 'foo'], s
  end

  test "puts" do
    s = script { puts 'lala' }
    assert_equal [:puts => 'lala'], s
  end
end
