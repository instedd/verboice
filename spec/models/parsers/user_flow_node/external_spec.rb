require 'spec_helper'

module Parsers
  module UserFlowNode
    describe External do

      let(:call_flow) { CallFlow.make }
      let(:external_service) { external_service_step.external_service }

      describe "variables type" do
        let(:external_service_step) { ExternalServiceStep.make :kind => 'callback', :response_type => 'variables'}

        it "should compile to an equivalent flow" do
          external = External.new call_flow, 'id' => 1,
            'type' => 'external',
            'name' => 'External Service',
            'external_step_id' => external_service_step.id

          external.equivalent_flow.first.should eq(
            Compiler.parse do |c|
              c.Label 1
              c.Assign 'current_step', 1
              c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'External Service', store: %("Calling External Service #{external_service.name}.")
              c.Callback external_service_step.callback_url, {:response_type => :variables, :external_service_id => external_service.id}
            end.first
          )
        end

        it "should compile with variables definitions" do
          external = External.new call_flow, 'id' => 1,
            'type' => 'external',
            'name' => 'External Service',
            'external_step_id' => external_service_step.id,
            'settings' => [
              {'name' => 'variable_with_step', 'step' => 20},
              {'name' => 'variable_with_variable', 'variable' => 'foobar'},
              {'name' => 'variable_with_value', 'value' => 'fixed value'}
            ]

          external.equivalent_flow.first.should eq(
            Compiler.parse do |c|
              c.Label 1
              c.Assign 'current_step', 1
              c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'External Service', store: %("Calling External Service #{external_service.name}.")
              c.RetrieveVariable 'foobar'
              c.Callback external_service_step.callback_url, {:response_type => :variables, :variables => {
                'variable_with_step' => 'value_20',
                'variable_with_variable' => 'foobar',
                'variable_with_value' => "'fixed value'"
              }, :external_service_id => external_service.id}
            end.first
          )
        end
      end

      describe "flow type" do
        let(:external_service_step) { ExternalServiceStep.make :kind => 'callback', :response_type => 'flow'}

        it "should compile to an equivalent flow" do
          external = External.new call_flow, 'id' => 1,
            'type' => 'external',
            'name' => 'External Service',
            'external_step_id' => external_service_step.id

          external.equivalent_flow.first.should eq(
            Compiler.parse do |c|
              c.Label 1
              c.Assign 'current_step', 1
              c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'External Service', store: %("Calling External Service #{external_service.name}.")
              c.Callback external_service_step.callback_url, {:response_type => :flow, :external_service_id => external_service.id}
            end.first
          )
        end
      end

      describe "none type" do
        let(:external_service_step) { ExternalServiceStep.make :kind => 'callback', :response_type => 'none'}

        it "should compile to an equivalent flow" do
          external = External.new call_flow, 'id' => 1,
            'type' => 'external',
            'name' => 'External Service',
            'external_step_id' => external_service_step.id

          external.equivalent_flow.first.should eq(
            Compiler.parse do |c|
              c.Label 1
              c.Assign 'current_step', 1
              c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'External Service', store: %("Calling External Service #{external_service.name}.")
              c.Callback external_service_step.callback_url, {:response_type => :none, :external_service_id => external_service.id}
            end.first
          )
        end
      end

    end
  end
end
