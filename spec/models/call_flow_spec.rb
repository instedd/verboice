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

describe CallFlow do

  context "callbacks" do
    it "sets name to callback url if name is empty" do
      call_flow = CallFlow.make :name => nil, :callback_url => 'foo', :mode => :callback_url
      call_flow.name.should == call_flow.callback_url
    end

    it "keeps name if name set" do
      call_flow = CallFlow.make :name => 'bar', :callback_url => 'foo', :mode => :callback_url
      call_flow.name.should == 'bar'
    end
  end

  context "commands" do
    before(:each) do
      @call_flow = CallFlow.make_unsaved
    end

    it "commands is flow when present" do
      @call_flow.flow = Commands::AnswerCommand.new
      @call_flow.commands.should == @call_flow.flow
    end

    it "commands when callback url is present" do
      @call_flow.callback_url = 'http://example.com'
      @call_flow.commands.should == Compiler.make { |b| b.Answer; b.Callback(@call_flow.callback_url) }
    end
  end

  it "should save its flow" do
    call_flow = CallFlow.make_unsaved
    call_flow.flow = Compiler.make { PlayUrl 'foo' }
    call_flow.save!

    call_flow.reload
    call_flow.flow.should == Compiler.make { PlayUrl 'foo' }
  end

  it "should update the flow when it's user flow get's updated" do
    call_flow = CallFlow.make id: 4
    call_flow.flow.should be_nil
    call_flow.user_flow = [
      {
        'id' => 1,
        'root' => 1,
        'type' => 'play',
        'name' => 'Play number one',
        'message' => {
          "name" => "Some explanation message",
          "type" => "text"
        }
      }
    ]

    call_flow.save!
    call_flow.reload.flow.should eq(
      Compiler.make do
        Answer()
        Assign "current_step", 1
        AssignValue "current_step_name", "Play number one"
        Trace call_flow_id: 4, step_id: 1, step_name: 'Play number one', store: '"Message played."'
        Say "Some explanation message"
      end
    )
  end

  it "should provide an error flow" do
    call_flow = CallFlow.make id: 4
    call_flow.error_flow.should eq(
      Compiler.make do
        Trace call_flow_id: 4, step_id: 'current_step', step_name: '', store: '"User hung up."'
      end
    )
  end

  it "should store its user flow's defined variables" do
    call_flow = CallFlow.make
    call_flow.user_flow = [{
      'id' => 1,
      'root' => true,
      'type' => 'capture',
      'name' => 'Capture number one',
      'store' => 'some_variable',
      'instructions_message' => { "name" => 'First Capture', 'type' => 'text' },
      'invalid_message' => {
        "name" => "An invalid key was pressed",
        "type" => "recording",
        "file" => "file.wav",
        "duration" => 5
      },
      'valid_values' => '1,2-4,10-20',
      'finish_on_key' => '#',
      'min_input_length' => 1,
      'max_input_length' => 2,
      'timeout' => 10 }]
    call_flow.save!

    call_flow.reload
    call_flow.variables.should eq(['some_variable'])
  end

  it "should store its user flow's external steps" do
    service = ExternalService.make
    step = ExternalServiceStep.make :external_service => service

    call_flow = CallFlow.make
    call_flow.user_flow = [
      {
        "id" => '1339774522765',
        "name" => "Play",
        "type" => "play",
        "root" => true,
        "next" => '1339802366354',
        "message" => {
            "name" => "Hello!",
            "type" => "text",
            "title" => "Message"
            }
      },
      {
        "id" => '1339802366354',
        "name" => "Analysis Results",
        "type" => "external",
        "root" => false,
        "next" => nil,
        "external_step_guid" => step.guid,
        "settings" => [{
          "step" => nil,
          "variable" => "foo",
          "value" => nil,
          "response" => nil,
          "name" => "pin",
          "display_name" => "Patient pin"
        }],
        "responses" => [{
          "name" => "result",
          "variable" => "bar"
        }]
      }
    ]
    call_flow.save!

    call_flow.reload
    call_flow.external_service_guids.should eq([service.guid])
  end
end
