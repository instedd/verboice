# Copyright (C) 2010-2012, InSTEDD
# 
# This file is part of Verboice.
# 
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

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
