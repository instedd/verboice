require 'spec_helper'

describe Voxeo::CallManager do
  
  let(:session_id) { 321 }
  let(:call_manager) { Voxeo::CallManager.new session_id }
  
  it "should tell session id" do
    call_manager.session_id.should eq(session_id)
  end
  
  it "defaults answering machine to false" do
    call_manager.is_answering_machine?.should be_false
  end

end