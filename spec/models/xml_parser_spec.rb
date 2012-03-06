require 'spec_helper'

describe XmlParser do
  it "parse TwiML" do
    commands = XmlParser.parse('<Response><Hangup/></Response>')
    commands.should == [:hangup]
  end

  it "parse invalid XML" do
    assert_raise(Exception, "Failed to parse the XML: Start tag expected, '<' not found") { XmlParser.parse('foo') }
  end

  it "parse unknown XML" do
    assert_raise(Exception, "Failed to parse the XML: unknown format") { XmlParser.parse('<foo/>') }
  end

  it "parse TwiML with invalid command" do
    assert_raise(Exception, "Failed to parse the XML: Invalid element 'Foo'") { XmlParser.parse('<Response><Foo/></Response>') }
  end

  it "should parse formatted TwiML XML (e.g. generated from haml)" do
    commands = XmlParser.parse("<?xml version='1.0' encoding='utf-8' ?>\n<Response>\n  <Say>\n    Hello World\n  </Say>\n  <Hangup></Hangup>\n</Response>\n")
    commands.should == [{:say=>"Hello World"}, :hangup]
  end
end
