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

describe Compiler do

  it "makes answer command" do
    result = subject.Answer.make
    expect(result).to be_instance_of(Commands::AnswerCommand)
  end

  it "makes assign expression command" do
    result = subject.Assign('foo', 'bar').make
    expect(result).to be_instance_of(Commands::AssignExpressionCommand)
    expect(result.name).to eq('foo')
    expect(result.data).to eq('bar')
  end

  it "makes assign value command" do
    result = subject.AssignValue('foo', 'bar').make
    expect(result).to be_instance_of(Commands::AssignValueCommand)
    expect(result.name).to eq('foo')
    expect(result.data).to eq('bar')
  end

  it "makes bridge command" do
    result = subject.Bridge(123).make
    expect(result).to be_instance_of(Commands::BridgeCommand)
    expect(result.session_id).to eq(123)
  end

  it "makes callback command" do
    result = subject.Callback('http://www.url.com', :method => 'get', :params => {:foo => 1}).make
    expect(result).to be_instance_of(Commands::CallbackCommand)
    expect(result.options[:url]).to eq('http://www.url.com')
    expect(result.options[:method]).to eq('get')
    expect(result.options[:params]).to eq({:foo => 1})
  end

  it "makes capture command" do
    result = subject.Capture.make
    expect(result).to be_instance_of(Commands::CaptureCommand)
  end

  it "makes dial command" do
    result = subject.Dial('123', :channel => 'channel').make
    expect(result).to be_instance_of(Commands::DialCommand)
    expect(result.number).to eq('123')
    expect(result.channel_name).to eq('channel')
  end

  it "makes hangup command" do
    result = subject.Hangup.make
    expect(result).to be_instance_of(Commands::HangupCommand)
  end

  it "makes pause command" do
    result = subject.Pause(5).make
    expect(result).to be_instance_of(Commands::PauseCommand)
    expect(result.length).to eq(5)
  end

  it "makes play url command" do
    result = subject.PlayUrl('http://foo.com').make
    expect(result).to be_instance_of(Commands::PlayUrlCommand)
    expect(result.url).to eq('http://foo.com')
  end

  it "makes say command" do
    result = subject.Say('hello').make
    expect(result).to be_instance_of(Commands::SayCommand)
    expect(result.text).to eq('hello')
  end

  it "makes trace command" do
    result = subject.Trace(project_id: 1, step_id: 2, step_name: 'foo', store: 'bar').make
    expect(result).to be_instance_of(Commands::TraceCommand)
  end

  it "makes persist variable command" do
    result = subject.PersistVariable('hello', 1).make
    expect(result).to be_instance_of(Commands::PersistVariableCommand)
    expect(result.variable_name).to eq('hello')
    expect(result.expression).to eq(1)
  end

  it "concatenates commands" do
    result = subject
      .Pause
      .Say('Foo')
      .make
    expect(result).to be_instance_of(Commands::PauseCommand)
    expect(result.next).to be_instance_of(Commands::SayCommand)
  end

  it "can append other builder" do
    result = subject
      .append(Compiler.parse { Answer(); Pause() })
      .make { Hangup() }
    expect(result).to be_instance_of(Commands::AnswerCommand)
    expect(result.next).to be_instance_of(Commands::PauseCommand)
    expect(result.next.next).to be_instance_of(Commands::HangupCommand)
  end

  it "can append other builder and take it's gotos and labels" do
    result = subject
      .append(Compiler.parse { Answer(); Goto(2); Label('1'); Pause() })
      .make { Label(2); Say('a'); Goto('1'); Hangup() }
    expect(result).to be_instance_of( Commands::AnswerCommand )
    expect(result.next).to be_instance_of( Commands::SayCommand )
    expect(result.next.next).to be_instance_of( Commands::PauseCommand )
  end

  it "can append nil" do
    result = subject.parse { Hangup() }.append(nil).make
    expect(result).to be_instance_of(Commands::HangupCommand)
    expect(result.next).to be_nil
  end

  it "can collect persisted variables" do
    compiler = subject\
      .Answer().Say('hello')
      .PersistVariable('var_1', 'my_expr_1')
      .Say('goodbye')
      .PersistVariable('var_2', 'my_expr_2')
      .Hangup()
    expect(compiler).to have(2).variables
    var_1, var_2 = compiler.variables.to_a
    expect(var_1).to eq('var_1')
    expect(var_2).to eq('var_2')
  end

  it "can collect persisted variables without repeating" do
    compiler = subject\
      .Answer().Say('hello')
      .PersistVariable('var_1', 'my_expr_1')
      .Say('goodbye')
      .PersistVariable('var_1', 'my_expr_2')
      .Hangup()
    expect(compiler).to have(1).variables
    var_1 = compiler.variables.first
    expect(var_1).to eq('var_1')
  end

  context "makes if command" do
    it "with block" do
      result = subject.make do
        If('a > b') { Pause() }
      end
      expect(result).to be_instance_of(Commands::IfCommand)
      expect(result.next).to be_nil
      expect(result.then).to be_instance_of(Commands::PauseCommand)
      expect(result.then.next).to be_nil
    end

    it "with builder" do
      result = subject
        .If('a > b') { |b| b.Pause }
        .make
      expect(result).to be_instance_of(Commands::IfCommand)
      expect(result.next).to be_nil
      expect(result.then).to be_instance_of(Commands::PauseCommand)
    end

    it "with other builder" do
      result = subject
        .If('a > b', Compiler.new.Pause)
        .make
      expect(result).to be_instance_of(Commands::IfCommand)
      expect(result.next).to be_nil
      expect(result.then).to be_instance_of(Commands::PauseCommand)
    end

    it "with other command" do
      result = subject
        .If('a > b', Compiler.make { Pause() })
        .make
        expect(result).to be_instance_of(Commands::IfCommand)
        expect(result.next).to be_nil
        expect(result.then).to be_instance_of(Commands::PauseCommand)
    end

    it "with else block" do
      result = subject
        .If('a > b') { Pause() }
        .Else { Say('foo') }
        .make
      expect(result).to be_instance_of(Commands::IfCommand)
      expect(result.next).to be_nil
      expect(result.then).to be_instance_of(Commands::PauseCommand)
      expect(result.else).to be_instance_of(Commands::SayCommand)
    end

    it "with else block from other builder" do
      result = subject
        .If('a > b') { Pause() }
        .Else(Compiler.new.Say('foo'))
        .make
      expect(result).to be_instance_of(Commands::IfCommand)
      expect(result.next).to be_nil
      expect(result.then).to be_instance_of(Commands::PauseCommand)
      expect(result.else).to be_instance_of(Commands::SayCommand)
    end

    it "should point next commands" do
      result = subject
        .If('a > b') { Pause() }
        .Else { Say('foo') }
        .Hangup
        .make
      expect(result.next).to be_instance_of(Commands::HangupCommand)
      expect(result.then.next).to be(result.next)
      expect(result.else.next).to be(result.next)
    end
  end

  context "makes while command" do
    it "with block" do
      result = subject.make do
        While('a > b') { Pause() }
        Hangup()
      end
      expect(result).to be_instance_of(Commands::WhileCommand)
      expect(result.next).to be_instance_of(Commands::HangupCommand)
      expect(result.block).to be_instance_of(Commands::PauseCommand)
      expect(result.block.next).to be(result)
    end

    it "with other builder" do
      result = subject.make do
        While('a > b', Compiler.new.Pause)
        Hangup()
      end
      expect(result).to be_instance_of(Commands::WhileCommand)
      expect(result.next).to be_instance_of(Commands::HangupCommand)
      expect(result.block).to be_instance_of(Commands::PauseCommand)
      expect(result.block.next).to be(result)
    end

    it "with empty block" do
      result = subject.make do
        While ('a > b') { }
      end
      expect(result).to be_instance_of(Commands::WhileCommand)
      expect(result.block).to be(result)
      expect(result.next).to be_nil
    end
  end

  context "label and goto" do

    it "can produce empty flow" do
      result = subject.make do
        Goto 'foo'
        Label 'foo'
      end
      expect(result).to be_nil
    end

    it "can produce empty flow with unused labels at end" do
      result = subject.make do
        Goto 'foo'
        Label 'foo'
        Label 'bar'
      end
      expect(result).to be_nil
    end

    it "can jump to beginning" do
      result = subject.make do
        Label('foo')
        Pause()
        Goto('foo')
      end
      expect(result).to be_instance_of(Commands::PauseCommand)
      expect(result.next).to be(result)
    end

    it "can skip commands" do
      result = subject.make do
        Answer()
        Goto('foo')
        Pause()
        Label('foo')
        Hangup()
      end
      expect(result).to be_instance_of(Commands::AnswerCommand)
      expect(result.next).to be_instance_of(Commands::HangupCommand)
      expect(result.next.next).to be_nil
    end

    it "can skip from beginning" do
      result = subject.make do
        Goto('foo')
        Answer()
        Label('foo')
        Pause()
        Hangup()
      end
      expect(result).to be_instance_of(Commands::PauseCommand)
      expect(result.next).to be_instance_of(Commands::HangupCommand)
      expect(result.next.next).to be_nil
    end

    it "can skip everything" do
      result = subject.make do
        Goto('foo')
        Answer()
        Hangup()
        Label('foo')
      end
      expect(result).to be_nil
    end

    it "can jump to next line" do
      result = subject.make do
        Goto('foo')
        Label('foo')
        Hangup()
      end
      expect(result).to be_instance_of(Commands::HangupCommand)
      expect(result.next).to be_nil
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
      expect(result).to be_instance_of(Commands::AnswerCommand)
      expect(result.next).to be_instance_of(Commands::HangupCommand)
      expect(result.next.next).to be_nil
    end

    it "can jump from if block" do
      result = subject.make do
        If('a > b') { Goto('foo') }
        Answer()
        Label('foo')
        Hangup()
      end
      expect(result).to be_instance_of(Commands::IfCommand)
      expect(result.next).to be_instance_of(Commands::AnswerCommand)
      expect(result.next.next).to be_instance_of(Commands::HangupCommand)
      expect(result.then).to be(result.next.next)
    end

    it "can jump from if block to the next command" do
      result = subject.make do
        If ('a > b') { Goto 'foo' }
        Label 'foo'
        Hangup()
      end
      expect(result).to be_instance_of(Commands::IfCommand)
      expect(result.next).to be_instance_of(Commands::HangupCommand)
      expect(result.then).to be(result.next)
      expect(result.next.next).to be_nil
    end

    it "can jump from if block with unused labels" do
      result = subject.make do
        If ('a > b') { Goto 'foo' }
        Label 'foo'
        Label 'bar'
      end
      expect(result).to be_instance_of(Commands::IfCommand)
      expect(result.next).to be_nil
      expect(result.then).to be_nil
    end

    it "can jump from else block" do
      result = subject.make do
        Label('foo')
        If('a > b') { Hangup() }
        Else { Goto('foo') }
      end
      expect(result).to be_instance_of(Commands::IfCommand)
      expect(result.next).to be_nil
      expect(result.then).to be_instance_of(Commands::HangupCommand)
      expect(result.then.next).to be_nil
      expect(result.else).to be(result)
    end

    it "can jump from while block" do
      result = subject.make do
        While ('a > b') { Goto('end') }
        Label('end')
        Hangup()
      end
      expect(result).to be_instance_of(Commands::WhileCommand)
      expect(result.next).to be_instance_of(Commands::HangupCommand)
      expect(result.block).to be(result.next)
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
      expect(result).to be_instance_of(Commands::WhileCommand)
      expect(result.next).to be_instance_of(Commands::HangupCommand)
      expect(result.block).to be_instance_of(Commands::IfCommand)
      expect(result.block.then).to be(result.next)
      expect(result.block.next).to be_instance_of(Commands::SayCommand)
      expect(result.block.next.next).to be(result)
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
      expect(result).to be_instance_of(Commands::SayCommand)
      expect(result.next).to be_instance_of(Commands::WhileCommand)
      expect(result.next.block).to be(result)
      expect(result.next.next).to be_nil
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
      expect(result).to be_instance_of(Commands::WhileCommand)
      expect(result.block).to be_instance_of(Commands::SayCommand)
      expect(result.block.next).to be(result)
      expect(result.next).to be_nil
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
      expect(result).to be_instance_of(Commands::WhileCommand)
      expect(result.next).to be_instance_of(Commands::HangupCommand)
      expect(result.block).to be(result)
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
      expect(result).to be_instance_of(Commands::HangupCommand)
      expect(result.next).to be_nil
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
      expect(result).to be_instance_of(Commands::HangupCommand)
      expect(result.next).to be_nil
    end

    it "should accept end command" do
      result = subject.make do
        Answer()
        End()
        Hangup()
      end
      expect(result).to be_instance_of(Commands::AnswerCommand)
      expect(result.next).to be_nil
    end

    it "should check for invalid Goto labels" do
      expect {
        subject.make do
          Goto 'foo'
        end
      }.to raise_error Exception
    end
  end
end
