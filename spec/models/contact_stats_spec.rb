require 'spec_helper'

describe ContactStats do  
  describe '#self.stats' do
    let!(:project) { Project.make }
    let!(:contacts) { [project.contacts.make(addresses_attributes: [{address: '1111'}, {address: 'AAAA'}]), project.contacts.make(addresses_attributes: [{address: '2222'}, {address: 'BBBB'}]), project.contacts.make(addresses_attributes: [{address: '3333'}, {address: 'CCCC'}])] }    

    let!(:call_flow) { project.call_flows.make name: "Callflow" } 
    let!(:other_call_flow) { project.call_flows.make name: "Other callflow" } 

    let!(:channel) { Channels::Custom.make call_flow: call_flow, name: "Channel" }
    let!(:other_channel) { Channels::Custom.make call_flow: call_flow, name: "Other channel" }

    def called(address, options)
      Timecop.freeze options[:at] if options[:at]

      flow = options[:flow] ? options[:flow] : call_flow
      through = options[:through] ? options[:through] : channel
      state = options[:state] ? options[:state] : :failed

      flow.call_logs.make(address: address, channel: through, state: state)

      Timecop.return if options[:at]
    end

    it 'returns one stat object per given contact' do
      stats = ContactStats.for project

      stats.length.should eq contacts.length
      stats.map{|s| s.contact.id}.sort.should eq contacts.map(&:id).sort
    end

    it 'is empty if the user never called' do
      stats = ContactStats.for project

      [:first_call, :last_call, :last_call_flow_name, :last_used_channel, :last_successful_call, :last_call_flow_name].each do |stat|
        stats.map{|s| s.send(stat)}.should eq [nil, nil, nil]
      end      
    end

    it 'processes more than one call for the same address' do
      called '1111', at: Time.utc(2012, 1, 1, 0, 0, 0)
      called '1111', at: Time.utc(2013, 1, 1, 0, 0, 0)

      stats = ContactStats.for project
      stats.map{|s| s.first_call}.should eq [Time.utc(2012, 1, 1, 0, 0, 0), nil, nil]
      stats.map{|s| s.last_call}.should eq [Time.utc(2013, 1, 1, 0, 0, 0), nil, nil]      
    end

    it 'processes more than one call for the same contact but different addresses' do
      called 'AAAA', at: Time.utc(2012, 1, 1, 0, 0, 0)
      called '1111', at: Time.utc(2013, 1, 1, 0, 0, 0)

      stats = ContactStats.for project
      stats.map{|s| s.first_call}.should eq [Time.utc(2012, 1, 1, 0, 0, 0), nil, nil]
      stats.map{|s| s.last_call}.should eq [Time.utc(2013, 1, 1, 0, 0, 0), nil, nil]
    end

    it 'processes more than one call for the same contact but different call flows' do
      called '1111', at: Time.utc(2012, 1, 1, 0, 0, 0), flow: call_flow
      called '1111', at: Time.utc(2013, 1, 1, 0, 0, 0), flow: other_call_flow

      stats = ContactStats.for project
      stats.map{|s| s.last_call_flow_name}.should eq ["Other callflow", nil, nil]
    end

    it 'processes more than one call for the same contact but different channels' do
      called '1111', at: Time.utc(2012, 1, 1, 0, 0, 0), through: channel
      called '1111', at: Time.utc(2013, 1, 1, 0, 0, 0), through: other_channel

      stats = ContactStats.for project
      stats.map{|s| s.last_used_channel}.should eq ["Other channel", nil, nil]
    end

    it 'processes successful and unsuccessful calls' do
      called '1111', at: Time.utc(2012, 1, 1, 0, 0, 0), state: :failed, flow: call_flow
      called '1111', at: Time.utc(2013, 1, 1, 0, 0, 0), state: :completed, flow: other_call_flow
      called '1111', at: Time.utc(2014, 1, 1, 0, 0, 0), state: :failed, flow: other_call_flow

      stats = ContactStats.for project
      stats.map{|s| s.first_call}.should eq [Time.utc(2012, 1, 1, 0, 0, 0), nil, nil]
      stats.map{|s| s.last_call}.should eq [Time.utc(2014, 1, 1, 0, 0, 0), nil, nil]      
      stats.map{|s| s.last_successful_call}.should eq [Time.utc(2013, 1, 1, 0, 0, 0), nil, nil]      
      stats.map{|s| s.last_successful_call_flow_name}.should eq ["Other callflow", nil, nil]      
    end
  end
end