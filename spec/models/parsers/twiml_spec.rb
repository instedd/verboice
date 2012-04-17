require 'spec_helper'

describe Parsers::Twiml do
  context "can parse?" do
    it "parse" do
      xml = Nokogiri.XML '<Response><Say>Hello</Say></Response>'
      Parsers::Twiml.can_parse?(xml).should_not be_nil
    end

    it "not parse" do
      xml = Nokogiri.XML '<phoneml></phoneml>'
      Parsers::Twiml.can_parse?(xml).should be_false
    end
  end

  it "parse play" do
    assert_parse '<Response><Play>http://foo</Play></Response>', Commands::PlayUrlCommand.new('http://foo')
  end

  context "gather" do
    def gather_commands(capture_options = {}, next_commands = nil, callback_options = {})
      callback_options[:params] = {:Digits => :digits}
      Compiler.make do
        Capture capture_options
        If('timeout || finish_key') { |c| c.append(next_commands) if next_commands }
        Else {
          Callback callback_options.delete(:url), callback_options
        }
      end
    end

    it "parse gather" do
      assert_parse '<Response><Gather/></Response>', gather_commands({:min => 1, :max => Float::INFINITY})
    end

    it "parse gather with attributes" do
      assert_parse '<Response><Gather timeout="3" finishOnKey="*" numDigits="4"/></Response>',
        gather_commands(:timeout => 3, :finish_on_key => "*", :min => 4, :max => 4)
    end

    it "parse gather with emedded play" do
      assert_parse '<Response><Gather><Play>http://foo</Play></Gather></Response>',
        gather_commands(:play => 'http://foo', :min => 1, :max => Float::INFINITY)
    end

    it "parse gather with emedded play" do
      assert_parse '<Response><Gather><Say>hello</Say></Gather></Response>',
        gather_commands(:say => 'hello', :min => 1, :max => Float::INFINITY)
    end

    it "parse gather with next commands on timeout/finish_key" do
      assert_parse '<Response><Gather/><Hangup /></Response>', gather_commands({:min => 1, :max => Float::INFINITY}, Commands::HangupCommand.new)
    end

    it "parse gather with callback options" do
      assert_parse '<Response><Gather action="http://www.domain.com/controller/action" method="GET"/></Response>',
        gather_commands({:min => 1, :max => Float::INFINITY}, nil, {:url => 'http://www.domain.com/controller/action', :method => 'GET'})
    end
  end

  context "redirect" do
    it "parse redirect" do
      assert_parse '<Response><Redirect>http://foo</Redirect></Response>', Commands::CallbackCommand.new('http://foo')
    end

    it "parse redirect with method" do
      assert_parse '<Response><Redirect method="get">http://foo</Redirect></Response>', Commands::CallbackCommand.new('http://foo', :method => 'get')
    end
  end

  it "parse say" do
    assert_parse '<Response><Say>Hello</Say></Response>', Commands::SayCommand.new('Hello')
  end

  it "parse hangup" do
    assert_parse '<Response><Hangup/></Response>', Commands::HangupCommand.new
  end

  it "parse pause" do
    assert_parse '<Response><Pause /></Response>', Commands::PauseCommand.new
  end

  it "parse pause with length" do
    assert_parse '<Response><Pause length="3"/></Response>', Commands::PauseCommand.new(3)
  end

  it "parse bridge" do
    assert_parse '<Response><Bridge session_id="123"/></Response>', Commands::BridgeCommand.new('123')
  end

  context "dial" do
    it "parse basic command" do
      assert_parse '<Response><Dial>1234</Dial></Response>', Commands::DialCommand.new('1234')
    end

    it "parse with channel" do
      assert_parse "<Response><Dial channel='foo'>1234</Dial></Response>", Commands::DialCommand.new('1234', :channel => 'foo')
    end

    it "parse with caller id" do
      assert_parse "<Response><Dial callerId='foo'>1234</Dial></Response>", Commands::DialCommand.new('1234', :caller_id => 'foo')
    end

    it "parse with action" do
      assert_parse "<Response><Dial action='http://foo'>1234</Dial></Response>", (
        Compiler.make do
          Dial '1234'
          Callback 'http://foo', :params => {:DialCallStatus => :dial_status}
        end)
    end

    it "parse with action and method" do
      assert_parse "<Response><Dial action='http://foo' method='get'>1234</Dial></Response>", (
        Compiler.make do
          Dial '1234'
          Callback 'http://foo', :method => 'get', :params => {:DialCallStatus => :dial_status}
        end)
    end

    it "ignore following commands if action is given" do
      assert_parse "<Response><Dial action='http://foo'>1234</Dial><Hangup/></Response>", (
        Compiler.make do
          Dial '1234'
          Callback 'http://foo', :params => {:DialCallStatus => :dial_status}
        end)
    end

    it "include following commands if action is not given" do
      assert_parse "<Response><Dial>1234</Dial><Hangup/></Response>", (
        Compiler.make do
          Dial '1234'
          Hangup()
        end)
    end
  end

  def assert_parse(xml, result)
    Parsers::Xml.parse(xml).should == result
  end
end
