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

describe Parsers::ExternalService do

  def parse(xml)
    existing = @existing_service || ExternalService.new(:url => 'http://www.domain.com')
    @service = Parsers::ExternalService.new(existing).parse(xml)
  end

  def service
    @service
  end

  context "creation" do

    it "should create a new external service with no steps" do
      parse <<-XML
        <verboice-service>
          <name>Empty service</name>
        </verboice-service>
      XML

      expect(service.name).to eq('Empty service')
      expect(service.steps).to be_empty
      expect(service.global_variables).to be_empty
      expect(service).to be_valid
    end

    it "should create a new external service with a callback step" do
      parse <<-XML
        <verboice-service>
          <name>My service</name>
          <steps>
            <step name="my-step"
              display-name="My step"
              icon="http://example.com/icon.png"
              type="callback"
              callback-url="http://example.com/callback/">
            </step>
          </steps>
        <verboice-service>
      XML

      expect(service.name).to eq('My service')
      expect(service.steps).to have(1).item
      expect(service).to be_valid

      step = service.steps.first
      expect(step.name).to eq('my-step')
      expect(step.display_name).to eq('My step')
      expect(step.icon).to eq('http://example.com/icon.png')
      expect(step.kind).to eq('callback')
      expect(step.callback_url).to eq('http://example.com/callback/')
      expect(step).not_to be_async
    end

    it "should create a new external service with an async callback step" do
      parse <<-XML
        <verboice-service>
          <name>My service</name>
          <steps>
            <step name="my-step"
              display-name="My step"
              icon="http://example.com/icon.png"
              type="callback"
              callback-url="http://example.com/callback/"
              async="true">
            </step>
          </steps>
        <verboice-service>
      XML

      expect(service.name).to eq('My service')
      expect(service.steps).to have(1).item
      expect(service).to be_valid

      step = service.steps.first
      expect(step.name).to eq('my-step')
      expect(step.display_name).to eq('My step')
      expect(step.icon).to eq('http://example.com/icon.png')
      expect(step.kind).to eq('callback')
      expect(step.callback_url).to eq('http://example.com/callback/')
      expect(step).to be_async
    end


    it "should create a new external service with a callback step with several variables" do
      parse <<-XML
        <verboice-service>
          <name>My service</name>
          <steps>
            <step name="my-step"
              display-name="My step"
              icon="http://example.com/icon.png"
              type="callback"
              callback-url="http://example.com/callback/">
              <settings>
                <variable name="myvar1" display-name="Variable One" type="string"/>
                <variable name="myvar2" display-name="Variable Two" type="numeric"/>
                <session_variable name="myvar3" />
              </settings>
            </step>
          </steps>
        <verboice-service>
      XML

      expect(service.steps).to have(1).item
      expect(service).to be_valid

      step = service.steps.first
      expect(step.name).to eq('my-step')

      expect(step).to have(2).variables
      var_1, var_2 = step.variables

      expect(var_1.name).to eq('myvar1')
      expect(var_1.display_name).to eq('Variable One')
      expect(var_1.type).to eq('string')

      expect(var_2.name).to eq('myvar2')
      expect(var_2.display_name).to eq('Variable Two')
      expect(var_2.type).to eq('numeric')

      expect(step).to have(1).session_variables
      expect(step.session_variables.first).to eq('myvar3')
    end

    it "should create a new external service with a callback step with several responses" do
      parse <<-XML
        <verboice-service>
          <name>My service</name>
          <steps>
            <step name="my-step"
              display-name="My step"
              icon="http://example.com/icon.png"
              type="callback"
              callback-url="http://example.com/callback/">
              <settings>
                <variable name="myvar1" display-name="Variable One" type="string"/>
              </settings>
              <response type="variables">
                <variable name="myresp1" display-name="Response One" type="string"/>
                <variable name="myresp2" display-name="Response Two" type="numeric"/>
              </response>
            </step>
          </steps>
        <verboice-service>
      XML

      expect(service.steps).to have(1).item
      expect(service).to be_valid

      step = service.steps.first
      expect(step.name).to eq('my-step')

      expect(step).to have(2).response_variables
      var_1, var_2 = step.response_variables

      expect(var_1.name).to eq('myresp1')
      expect(var_1.display_name).to eq('Response One')
      expect(var_1.type).to eq('string')

      expect(var_2.name).to eq('myresp2')
      expect(var_2.display_name).to eq('Response Two')
      expect(var_2.type).to eq('numeric')
    end


    it "should create a new external service with a callback step with flow response type" do
      parse <<-XML
        <verboice-service>
          <name>My service</name>
          <steps>
            <step name="my-step"
              display-name="My step"
              icon="http://example.com/icon.png"
              type="callback"
              callback-url="http://example.com/callback/">
              <settings>
                <variable name="myvar1" display-name="Variable One" type="string"/>
              </settings>
              <response type="flow"/>
            </step>
          </steps>
        <verboice-service>
      XML

      expect(service.steps).to have(1).item
      expect(service).to be_valid

      step = service.steps.first
      expect(step.name).to eq('my-step')
      expect(step.response_type).to eq('flow')
      expect(step).to have(0).response_variables
    end


    it "should create a new external service with global settings" do
      parse <<-XML
        <verboice-service>
          <name>My Service</name>
          <global-settings>
            <variable name="globalvar1" display-name="Global Var One" type="string"/>
            <variable name="globalvar2" display-name="Global Var Two" type="numeric"/>
          </global-settings>
        </verboice-service>
      XML

      expect(service.global_variables).to have(2).item
      expect(service).to be_valid

      global_var_1 = service.global_variables.first
      expect(global_var_1.name).to eq('globalvar1')
      expect(global_var_1.display_name).to eq('Global Var One')
      expect(global_var_1.type).to eq('string')
      expect(global_var_1.value).to be_nil

      global_var_2 = service.global_variables.last
      expect(global_var_2.name).to eq('globalvar2')
      expect(global_var_2.display_name).to eq('Global Var Two')
      expect(global_var_2.type).to eq('numeric')
      expect(global_var_2.value).to be_nil
    end

    it "should not create a new external service with invalid variable names" do
      parse <<-XML
        <verboice-service>
          <name>My service</name>
          <steps>
            <step name="my-step"
              display-name="My step"
              icon="http://example.com/icon.png"
              type="callback"
              callback-url="http://example.com/callback/">
              <settings>
                <variable name="my-var-1" display-name="Variable One" type="string"/>
              </settings>
              <response type="flow"/>
            </step>
          </steps>
        <verboice-service>
      XML

      expect(service.steps).to have(1).item
      expect(service.steps.first).to be_invalid
      expect(service).to be_invalid
    end

  end

  it "should create a new external service with a callback step" do
    parse <<-XML
      <verboice-service>
        <name>My service</name>
        <steps>
          <step name="my-step"
            display-name="My step"
            icon="http://example.com/icon.png"
            type="script">
            <script><![CDATA[1]]></script>
          </step>
        </steps>
      <verboice-service>
    XML

    expect(service.name).to eq('My service')
    expect(service.steps).to have(1).item
    expect(service).to be_valid

    step = service.steps.first
    expect(step.name).to eq('my-step')
    expect(step.display_name).to eq('My step')
    expect(step.icon).to eq('http://example.com/icon.png')
    expect(step.kind).to eq('script')
    expect(step.script).to eq('1')
  end

  it "should use base_url attribute to build absolute_callback_url if relative callback_url" do
    parse <<-XML
      <verboice-service>
        <name>My service</name>
        <steps>
          <step name="my-step"
            display-name="My step"
            icon="http://example.com/icon.png"
            type="callback"
            callback-url="/callback">
          </step>
        </steps>
      <verboice-service>
    XML

    expect(service.name).to eq('My service')
    expect(service.steps).to have(1).item
    service.base_url = 'http://mybaseurl.com'
    expect(service).to be_valid

    step = service.steps.first
    step.external_service = service
    expect(step.name).to eq('my-step')
    expect(step.display_name).to eq('My step')
    expect(step.icon).to eq('http://example.com/icon.png')
    expect(step.kind).to eq('callback')
    expect(step.absolute_callback_url).to eq('http://mybaseurl.com/callback')
    expect(step).not_to be_async
  end

  it "should not use base_url attribute to build absolute_callback_url if callback_url is absolute" do
    parse <<-XML
      <verboice-service>
        <name>My service</name>
        <steps>
          <step name="my-step"
            display-name="My step"
            icon="http://example.com/icon.png"
            type="callback"
            callback-url="http://othersite.com/callback">
          </step>
        </steps>
      <verboice-service>
    XML

    expect(service.name).to eq('My service')
    expect(service.steps).to have(1).item
    service.base_url = 'http://mybaseurl.com'
    expect(service).to be_valid

    step = service.steps.first
    step.external_service = service
    expect(step.name).to eq('my-step')
    expect(step.display_name).to eq('My step')
    expect(step.icon).to eq('http://example.com/icon.png')
    expect(step.kind).to eq('callback')
    expect(step.absolute_callback_url).to eq('http://othersite.com/callback')
    expect(step).not_to be_async
  end

  it "should allow variables interpolation in url" do
    parse <<-XML
      <verboice-service>
        <name>My service</name>
        <global-settings>
          <variable name="service_domain" display-name="Service Domain" type="string"/>
        </global-settings>
        <steps>
          <step name="my-step"
            display-name="My step"
            icon="http://example.com/icon.png"
            type="callback"
            callback-url="http://{service_domain}/callback">
          </step>
        </steps>
      <verboice-service>
    XML

    expect(service.name).to eq('My service')
    expect(service.steps).to have(1).item
    service.base_url = 'http://mybaseurl.com'
    expect(service).to be_valid

    step = service.steps.first
    step.external_service = service
    expect(step.name).to eq('my-step')
    expect(step.display_name).to eq('My step')
    expect(step.icon).to eq('http://example.com/icon.png')
    expect(step.kind).to eq('callback')
    expect(step.absolute_callback_url).to eq('http://{service_domain}/callback')
    expect(step).not_to be_async
  end

  context "updating" do

    before(:each) do
      @existing_service = ExternalService.make :name => "Old name"
    end

    it "should update an existing service with a new name" do
      parse <<-XML
        <verboice-service>
          <name>Empty service</name>
        </verboice-service>
      XML

      expect(service.name).to eq('Empty service')
      expect(service).to be_valid
    end

    it "should update an existing service creating a new step" do
      parse <<-XML
        <verboice-service>
          <name>My service</name>
          <steps>
            <step name="my-step"
              display-name="My step"
              icon="http://example.com/icon.png"
              type="callback"
              callback-url="http://example.com/callback/">
            </step>
          </steps>
        <verboice-service>
      XML

      expect(service.name).to eq('My service')
      expect(service.steps).to have(1).item
      expect(service).to be_valid

      step = service.steps.first
      expect(step.name).to eq('my-step')
      expect(step.display_name).to eq('My step')
      expect(step.icon).to eq('http://example.com/icon.png')
      expect(step.callback_url).to eq('http://example.com/callback/')
    end

    it "should update an existing service creating a new step, updating an existing one and deleting another existing one" do
      @existing_service.steps.create :name => "step_to_be_updated", :display_name => 'To be updated', :icon => "http://example.com/old-icon.png"
      to_be_deleted = @existing_service.steps.create :name => "step_to_be_deleted"

      parse <<-XML
        <verboice-service>
          <name>My service</name>
          <steps>
            <step name="step_to_be_created" display-name="New step"/>
            <step name="step_to_be_updated" icon="http://example.com/icon.png"/>
          </steps>
        <verboice-service>
      XML

      expect(service).to be_valid
      service.save!
      expect(service.reload.steps).to have(2).items

      updated_step = service.steps.first
      expect(updated_step.name).to eq('step_to_be_updated')
      expect(updated_step.display_name).to eq('To be updated')
      expect(updated_step.icon).to eq('http://example.com/icon.png')

      created_step = service.steps.last
      expect(created_step.name).to eq('step_to_be_created')
      expect(created_step.display_name).to eq('New step')

      expect(service.steps.where(:id => to_be_deleted.id)).to have(0).items
    end

    context "global settings" do
      before(:each) do
        globar_var_1 = ExternalService::GlobalVariable.new.tap do |v|
          v.name = 'globalvar1'
          v.display_name =  'Global Var One'
          v.type = 'string'
          v.value = 'global_var_1_value'
        end

        @existing_service.global_variables << globar_var_1
      end

      it "should keep values of variables and update fields" do
        parse <<-XML
          <verboice-service>
            <name>My Service</name>
            <global-settings>
              <variable name="globalvar1" display-name="Updated Global Var One" type="numeric"/>
            </global-settings>
          </verboice-service>
        XML

        expect(service.global_variables).to have(1).items

        updated_global_var_1 = service.global_variables.first
        expect(updated_global_var_1.name).to eq('globalvar1')
        expect(updated_global_var_1.display_name).to eq('Updated Global Var One')
        expect(updated_global_var_1.type).to eq('numeric')
        expect(updated_global_var_1.value).to eq('global_var_1_value')
      end

      it "should delete removed variables" do
        parse <<-XML
          <verboice-service>
            <name>My Service</name>
            <global-settings>
              <variable name="globalvar2" display-name="Global Var Two" type="numeric"/>
            </global-settings>
          </verboice-service>
        XML

        expect(service.global_variables).to have(1).items
        expect(service.global_variables.detect{|v| v.name == 'globalvar1'}).to be_nil
      end

      it "should add new variables" do
        parse <<-XML
          <verboice-service>
            <name>My Service</name>
            <global-settings>
              <variable name="globalvar1" display-name="Global Var One" type="string"/>
              <variable name="globalvar2" display-name="Global Var Two" type="numeric"/>
            </global-settings>
          </verboice-service>
        XML

        expect(service.global_variables).to have(2).items

        global_var_2 = service.global_variables.detect{|v| v.name == 'globalvar2'}
        expect(global_var_2.name).to eq('globalvar2')
        expect(global_var_2.display_name).to eq('Global Var Two')
        expect(global_var_2.type).to eq('numeric')
        expect(global_var_2.value).to be_nil
      end
    end

  end
end
