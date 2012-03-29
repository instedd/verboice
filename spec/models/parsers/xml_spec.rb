require 'spec_helper'

describe Parsers::Xml do
  it "parse TwiML" do
    commands = Parsers::Xml.parse('<Response><Hangup/></Response>')
    commands.should == [:hangup]
  end

  it "parse invalid XML" do
    assert_raise(Exception, "Failed to parse the XML: Start tag expected, '<' not found") { Parsers::Xml.parse('foo') }
  end

  it "parse unknown XML" do
    assert_raise(Exception, "Failed to parse the XML: unknown format") { Parsers::Xml.parse('<foo/>') }
  end

  it "parse TwiML with invalid command" do
    assert_raise(Exception, "Failed to parse the XML: Invalid element 'Foo'") { Parsers::Xml.parse('<Response><Foo/></Response>') }
  end
end
