require 'spec_helper'

module Parsers
  module UserFlowNode
    describe Transfer do

      let(:call_flow) { double('call_flow', :id => 1) }

      it "should compile to an equivalent flow" do
        transfer = Transfer.new call_flow, 'id' => 1,
          'type' => 'transfer',
          'name' => 'Transfer',
          'channel' => 'foo',
          'address' => '1234-5678'

        transfer.equivalent_flow.first.should eq(
          Compiler.parse do
            Label 1
            Assign "current_step", 1
            Trace call_flow_id: 1, step_id: 1, step_name: 'Transfer', store: '"Transfer to 1234-5678 in channel foo."'
            Dial '1234-5678', {:channel => 'foo'}
          end.first
        )
      end

      it "should compile with current channel" do
        transfer = Transfer.new call_flow, 'id' => 2,
          'type' => 'transfer',
          'name' => 'Transfer',
          'address' => '1234-5678'

        transfer.equivalent_flow.first.should eq(
          Compiler.parse do
            Label 2
            Assign "current_step", 2
            Trace call_flow_id: 1, step_id: 2, step_name: 'Transfer', store: '"Transfer to 1234-5678 in current channel."'
            Dial '1234-5678', {:channel => nil}
          end.first
        )
      end

    end
  end
end