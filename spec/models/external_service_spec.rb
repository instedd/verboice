# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

require 'spec_helper'

describe ExternalService do

  let(:external_service) { ExternalService.new }

  it 'updates the manifest' do
    external_service.url = 'http://service-url.com'
    xml = '<verboice-service><name>my_service</name></verboice-service>'
    response = double('response', :to_str => xml)
    expect(RestClient).to receive(:get).with(external_service.url).and_return(response)

    external_service.update_manifest!
    expect(external_service.reload.xml).to eq(xml)
  end

  it 'has empty global variables upon creation' do
    expect(external_service.global_variables).to be_empty
  end

  describe 'global settings' do
    let(:variable) { ExternalService::GlobalVariable.new :name => 'var_name_1', :value => 'var_value_1' }

    before(:each) do
      external_service.global_variables << variable
    end

    it 'updates global variables' do
      attrs = {"0" => {:name => 'var_name_1', :value => 'new_var_value_1'}, "1" => {:name => 'var_name_2', :value => 'new_var_value_2'}}

      external_service.global_variables_attributes = attrs

      expect(external_service.global_variables).to have(1).items
      variable = external_service.global_variables.first
      expect(variable.value).to eq('new_var_value_1')
    end

    it 'should return global variables value' do
      expect(external_service.global_variable_value_for('var_name_1')).to eq('var_value_1')
      expect(external_service.global_variable_value_for('var_name_2')).to be_nil
    end
  end

  describe 'clean call flows' do
    before(:each) do
      allow(external_service).to receive(:call_flows).and_return([double('call_flow_1'), double('call_flow_2')])
    end

    it 'should clean associated call flows' do
      external_service.call_flows.each do |call_flow|
        expect(call_flow).to receive(:clean_external_service).with(external_service)
        expect(call_flow).to receive(:save!)
      end

      external_service.clean_call_flows
    end
  end

end
