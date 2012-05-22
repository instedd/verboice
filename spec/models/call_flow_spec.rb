require 'spec_helper'

describe CallFlow do

  context "callbacks" do
    it "sets name to callback url if name is empty" do
      call_flow = CallFlow.make :name => nil, :callback_url => 'foo'
      call_flow.name.should == call_flow.callback_url
    end

    it "keeps name if name set" do
      call_flow = CallFlow.make :name => 'bar', :callback_url => 'foo'
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

  it "should save it's flow" do
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
        Trace call_flow_id: 4, step_id: 1, step_name: 'Play number one', store: '"Message played."'
        Say "Some explanation message"
      end
    )

    call_flow.error_flow.should eq(
      Compiler.make do
        Trace call_flow_id: 4, step_id: 'current_step', step_name: '', store: '"User hanged up."'
      end
    )
  end
end
