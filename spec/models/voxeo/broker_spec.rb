require 'spec_helper'

describe Voxeo::Broker do
  
  let(:broker) { Voxeo::Broker.new }
  let(:channel) { Channel.make(:voxeo) }
  
  it "defaults pbx available to true" do
    broker.pbx_available?.should be_true
  end
  
  context "call" do
    
    let(:session) { Session.new :channel => channel, :address => 'foo' }
    
    it "should make a call request" do
      expect_em_http :get, Voxeo::Broker::Url, :with => {:query => {:tokenid => channel.token, :callsid => session.id, :numbertodial => session.address}}
      broker.call session
    end
    
  end
end