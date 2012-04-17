require 'spec_helper'

describe Parsers::Xml do
  it "parse TwiML" do
    commands = Parsers::Xml.parse('<Response><Hangup/></Response>')
    commands.should == Commands::HangupCommand.new
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

  it "should parse formatted TwiML XML (e.g. generated from haml)" do
    commands = Parsers::Xml.parse("<?xml version='1.0' encoding='utf-8' ?>\n<Response>\n  <Say>\n    Hello World\n  </Say>\n  <Hangup></Hangup>\n</Response>\n")
    commands.should == Compiler.make { Say 'Hello World'; Hangup() }
  end

end
