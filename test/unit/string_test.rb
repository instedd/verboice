require 'test_helper'

class StringTest < ActiveSupport::TestCase
  test "dot extension no extension" do
    assert_equal '', 'foo'.dot_extension
  end

  test "dot extension one dot" do
    assert_equal '.txt', 'foo.txt'.dot_extension
  end

  test "dot extension two dots" do
    assert_equal '.txt', 'foo.bar.txt'.dot_extension
  end
end
