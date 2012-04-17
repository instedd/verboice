require 'spec_helper'

module Commands
  describe SayCommand do
    
    let(:pbx) { double('pbx') }
    let(:session) { Session.new :pbx => pbx}
    
    it "runs" do
      session.pbx.should_receive(:say).with('some text')
      session.should_receive(:info).with("Say 'some text'")
      Commands::SayCommand.new('some text').run session
    end
    
  end
end