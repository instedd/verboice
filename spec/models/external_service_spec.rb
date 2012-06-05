require 'spec_helper'

describe ExternalService do

  let(:external_service) { ExternalService.new }

  it 'updates the manifest' do
    external_service.url = 'service url'
    response = double('response', :to_str => 'new xml')
    RestClient.should_receive(:get).with('service url').and_return(response)

    external_service.update_manifest!

    external_service.reload.xml.should eq('new xml')
  end

  it 'has empty global variables upon creation' do
    external_service.global_variables.should be_empty
  end

  describe 'global settings' do
    let(:variable) { ExternalService::GlobalVariable.new :name => 'var_name_1', :value => 'var_value_1' }

    before(:each) do
      external_service.global_variables << variable
    end

    it 'updates global variables' do
      attrs = {"0" => {:name => 'var_name_1', :value => 'new_var_value_1'}, "1" => {:name => 'var_name_2', :value => 'new_var_value_2'}}

      external_service.global_variables_attributes = attrs

      external_service.global_variables.should have(1).items
      variable = external_service.global_variables.first
      variable.value.should eq('new_var_value_1')
    end

    it 'should return global variables value' do
      external_service.global_variable_value_for('var_name_1').should eq('var_value_1')
      external_service.global_variable_value_for('var_name_2').should be_nil
    end
  end

end
