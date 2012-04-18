require 'spec_helper'

describe Compiler do

  it "makes answer command" do
    result = subject.Answer.make
    result.should be_instance_of(Commands::AnswerCommand)
  end

  it "makes assign command" do
    result = subject.Assign('foo', 'bar').make
    result.should be_instance_of(Commands::AssignCommand)
    result.name.should == 'foo'
    result.expr.should == 'bar'
  end

  it "makes bridge command" do
    result = subject.Bridge(123).make
    result.should be_instance_of(Commands::BridgeCommand)
    result.session_id.should == 123
  end

  it "makes callback command" do
    result = subject.Callback('http://www.url.com', :method => 'get', :params => {:foo => 1}).make
    result.should be_instance_of(Commands::CallbackCommand)
    result.url.should == 'http://www.url.com'
    result.method.should == 'get'
    result.params.should == {:foo => 1}
  end

  it "makes capture command" do
    result = subject.Capture.make
    result.should be_instance_of(Commands::CaptureCommand)
  end

  it "makes dial command" do
    result = subject.Dial('123', :channel => 'channel').make
    result.should be_instance_of(Commands::DialCommand)
    result.number.should == '123'
    result.channel_name.should == 'channel'
  end

  it "makes hangup command" do
    result = subject.Hangup.make
    result.should be_instance_of(Commands::HangupCommand)
  end

  it "makes pause command" do
    result = subject.Pause(5).make
    result.should be_instance_of(Commands::PauseCommand)
    result.length.should == 5
  end

  it "makes play url command" do
    result = subject.PlayUrl('http://foo.com').make
    result.should be_instance_of(Commands::PlayUrlCommand)
    result.url.should == 'http://foo.com'
  end

  it "makes say command" do
    result = subject.Say('hello').make
    result.should be_instance_of(Commands::SayCommand)
    result.text.should == 'hello'
  end

  it "makes trace command" do
    result = subject.Trace(application_id: 1, step_id: 2, step_name: 'foo', store: 'bar').make
    result.should be_instance_of(Commands::TraceCommand)
  end

  it "concatenates commands" do
    result = subject
      .Pause
      .Say('Foo')
      .make
    result.should be_instance_of(Commands::PauseCommand)
    result.next.should be_instance_of(Commands::SayCommand)
  end

  it "can append other builder" do
    result = subject
      .append(Compiler.parse { Answer(); Pause() })
      .make { Hangup() }
    result.should be_instance_of(Commands::AnswerCommand)
    result.next.should be_instance_of(Commands::PauseCommand)
    result.next.next.should be_instance_of(Commands::HangupCommand)
  end

  it "can append nil" do
    result = subject.parse { Hangup() }.append(nil).make
    result.should be_instance_of(Commands::HangupCommand)
    result.next.should be_nil
  end

  context "makes if command" do
    it "with block" do
      result = subject.make do
        If('a > b') { Pause() }
      end
      result.should be_instance_of(Commands::IfCommand)
      result.next.should be_nil
      result.then.should be_instance_of(Commands::PauseCommand)
      result.then.next.should be_nil
    end

    it "with builder" do
      result = subject
        .If('a > b') { |b| b.Pause }
        .make
      result.should be_instance_of(Commands::IfCommand)
      result.next.should be_nil
      result.then.should be_instance_of(Commands::PauseCommand)
    end

    it "with else block" do
      result = subject
        .If('a > b') { Pause() }
        .Else { Say('foo') }
        .make
      result.should be_instance_of(Commands::IfCommand)
      result.next.should be_nil
      result.then.should be_instance_of(Commands::PauseCommand)
      result.else.should be_instance_of(Commands::SayCommand)
    end

    it "should point next commands" do
      result = subject
        .If('a > b') { Pause() }
        .Else { Say('foo') }
        .Hangup
        .make
      result.next.should be_instance_of(Commands::HangupCommand)
      result.then.next.should be(result.next)
      result.else.next.should be(result.next)
    end
  end

  context "makes while command" do
    it "with block" do
      result = subject.make do
        While('a > b') { Pause() }
        Hangup()
      end
      result.should be_instance_of(Commands::WhileCommand)
      result.next.should be_instance_of(Commands::HangupCommand)
      result.block.should be_instance_of(Commands::PauseCommand)
      result.block.next.should be(result)
    end

    it "with empty block" do
      result = subject.make do
        While ('a > b') { }
      end
      result.should be_instance_of(Commands::WhileCommand)
      result.block.should be(result)
      result.next.should be_nil
    end
  end

  context "label and goto" do

    it "can produce empty flow" do
      result = subject.make do
        Goto 'foo'
        Label 'foo'
      end
      result.should be_nil
    end

    it "can produce empty flow with unused labels at end" do
      result = subject.make do
        Goto 'foo'
        Label 'foo'
        Label 'bar'
      end
      result.should be_nil
    end

    it "can jump to beginning" do
      result = subject.make do
        Label('foo')
        Pause()
        Goto('foo')
      end
      result.should be_instance_of(Commands::PauseCommand)
      result.next.should be(result)
    end

    it "can skip commands" do
      result = subject.make do
        Answer()
        Goto('foo')
        Pause()
        Label('foo')
        Hangup()
      end
      result.should be_instance_of(Commands::AnswerCommand)
      result.next.should be_instance_of(Commands::HangupCommand)
      result.next.next.should be_nil
    end

    it "can skip from beginning" do
      result = subject.make do
        Goto('foo')
        Answer()
        Label('foo')
        Pause()
        Hangup()
      end
      result.should be_instance_of(Commands::PauseCommand)
      result.next.should be_instance_of(Commands::HangupCommand)
      result.next.next.should be_nil
    end

    it "can skip everything" do
      result = subject.make do
        Goto('foo')
        Answer()
        Hangup()
        Label('foo')
      end
      result.should be_nil
    end

    it "can jump to next line" do
      result = subject.make do
        Goto('foo')
        Label('foo')
        Hangup()
      end
      result.should be_instance_of(Commands::HangupCommand)
      result.next.should be_nil
    end

    it "can reorder commands" do
      result = subject.make do
        Goto('1')
        Label('2')
        Hangup()
        Goto('end')
        Label('1')
        Answer()
        Goto('2')
        Label('end')
      end
      result.should be_instance_of(Commands::AnswerCommand)
      result.next.should be_instance_of(Commands::HangupCommand)
      result.next.next.should be_nil
    end

    it "can jump from if block" do
      result = subject.make do
        If('a > b') { Goto('foo') }
        Answer()
        Label('foo')
        Hangup()
      end
      result.should be_instance_of(Commands::IfCommand)
      result.next.should be_instance_of(Commands::AnswerCommand)
      result.next.next.should be_instance_of(Commands::HangupCommand)
      result.then.should be(result.next.next)
    end

    it "can jump from if block to the next command" do
      result = subject.make do
        If ('a > b') { Goto 'foo' }
        Label 'foo'
        Hangup()
      end
      result.should be_instance_of(Commands::IfCommand)
      result.next.should be_instance_of(Commands::HangupCommand)
      result.then.should be(result.next)
      result.next.next.should be_nil
    end

    it "can jump from if block with unused labels" do
      result = subject.make do
        If ('a > b') { Goto 'foo' }
        Label 'foo'
        Label 'bar'
      end
      result.should be_instance_of(Commands::IfCommand)
      result.next.should be_nil
      result.then.should be_nil
    end

    it "can jump from else block" do
      result = subject.make do
        Label('foo')
        If('a > b') { Hangup() }
        Else { Goto('foo') }
      end
      result.should be_instance_of(Commands::IfCommand)
      result.next.should be_nil
      result.then.should be_instance_of(Commands::HangupCommand)
      result.then.next.should be_nil
      result.else.should be(result)
    end

    it "can jump from while block" do
      result = subject.make do
        While ('a > b') { Goto('end') }
        Label('end')
        Hangup()
      end
      result.should be_instance_of(Commands::WhileCommand)
      result.next.should be_instance_of(Commands::HangupCommand)
      result.block.should be(result.next)
    end

    it "can jump from if inside while block" do
      result = subject.make do
        While ('a > b') {
          If ('p > q') { Goto 'end' }
          Say('hello')
        }
        Label('end')
        Hangup()
      end
      result.should be_instance_of(Commands::WhileCommand)
      result.next.should be_instance_of(Commands::HangupCommand)
      result.block.should be_instance_of(Commands::IfCommand)
      result.block.then.should be(result.next)
      result.block.next.should be_instance_of(Commands::SayCommand)
      result.block.next.next.should be(result)
    end

    it "can jump inside block" do
      result = subject.make do
        Goto 'loop'
        Hangup()
        While ('a > b') {
          Label 'loop'
          Say 'hello'
        }
      end
      result.should be_instance_of(Commands::SayCommand)
      result.next.should be_instance_of(Commands::WhileCommand)
      result.next.block.should be(result)
      result.next.next.should be_nil
    end

    it "can jump to the end of while block" do
      result = subject.make do
        Goto 'loop'
        Hangup()
        While ('a > b') {
          Say 'hello'
          Label 'loop'
        }
      end
      result.should be_instance_of(Commands::WhileCommand)
      result.block.should be_instance_of(Commands::SayCommand)
      result.block.next.should be(result)
      result.next.should be_nil
    end

    it "can jump to empty while block" do
      result = subject.make do
        Goto 'loop'
        Hangup()
        While ('a > b') {
          Label 'loop'
        }
        Hangup()
      end
      result.should be_instance_of(Commands::WhileCommand)
      result.next.should be_instance_of(Commands::HangupCommand)
      result.block.should be(result)
    end

    it "can jump to end of if" do
      result = subject.make do
        Goto 'end'
        Answer()
        If ('a > b') {
          Say 'hello'
          Label 'end'
        }
        Hangup()
      end
      result.should be_instance_of(Commands::HangupCommand)
      result.next.should be_nil
    end

    it "can jump to end of else" do
      result = subject.make do
        Goto 'end'
        Answer()
        If ('a > b') {}
        Else {
          Say 'hello'
          Label 'end'
        }
        Hangup()
      end
      result.should be_instance_of(Commands::HangupCommand)
      result.next.should be_nil
    end
  end
end
