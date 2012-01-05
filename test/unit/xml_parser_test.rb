require 'test_helper'

class XmlParserTest < ActiveSupport::TestCase

  test "parse TwiML" do
    commands = XmlParser.parse('<Response><Hangup/></Response>')
    assert_equal [:hangup], commands
  end

  test "parse invalid XML" do
    assert_raise(Exception, "Failed to parse the XML: Start tag expected, '<' not found") { XmlParser.parse('foo') }
  end

  test "parse unknown XML" do
    assert_raise(Exception, "Failed to parse the XML: unknown format") { XmlParser.parse('<foo/>') }
  end

  test "parse TwiML with invalid command" do
    assert_raise(Exception, "Failed to parse the XML: Invalid element 'Foo'") { XmlParser.parse('<Response><Foo/></Response>') }
  end

end