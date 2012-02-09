require 'spec_helper'

describe JsCommand do
  before(:each) do
    @session = Session.new :pbx => mock('pbx'), :call_log => CallLog.new
  end

  it "answer" do
    @session.pbx.should_receive(:answer)

    cmd = JsCommand.new 'answer();'
    cmd.run @session
  end
end
