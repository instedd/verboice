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

  it 'has empty global setting upon creation' do
    external_service.global_settings.should be_empty
  end

  it 'updates global settings' do
    variable = ExternalService::GlobalVariable.new
    variable.name = 'var_name_1'
    variable.value = 'var_value_1'
    external_service.global_settings[variable.name] = variable

    attrs = {"0" => {:name => 'var_name_1', :value => 'new_var_value_1'}, "1" => {:name => 'var_name_2', :value => 'new_var_value_2'}}

    external_service.global_settings_attributes = attrs

    external_service.global_settings[variable.name].value.should eq('new_var_value_1')
    external_service.global_settings['var_name_2'].should be_nil
  end

end
