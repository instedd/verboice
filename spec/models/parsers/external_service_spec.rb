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

      service.name.should eq('Empty service')
      service.steps.should be_empty
      service.global_variables.should be_empty
      service.should be_valid
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

      service.name.should eq('My service')
      service.steps.should have(1).item
      service.should be_valid

      step = service.steps.first
      step.name.should eq('my-step')
      step.display_name.should eq('My step')
      step.icon.should eq('http://example.com/icon.png')
      step.kind.should eq('callback')
      step.callback_url.should eq('http://example.com/callback/')
      step.should_not be_async
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

      service.name.should eq('My service')
      service.steps.should have(1).item
      service.should be_valid

      step = service.steps.first
      step.name.should eq('my-step')
      step.display_name.should eq('My step')
      step.icon.should eq('http://example.com/icon.png')
      step.kind.should eq('callback')
      step.callback_url.should eq('http://example.com/callback/')
      step.should be_async
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

      service.steps.should have(1).item
      service.should be_valid

      step = service.steps.first
      step.name.should eq('my-step')

      step.should have(2).variables
      var_1, var_2 = step.variables

      var_1.name.should eq('myvar1')
      var_1.display_name.should eq('Variable One')
      var_1.type.should eq('string')

      var_2.name.should eq('myvar2')
      var_2.display_name.should eq('Variable Two')
      var_2.type.should eq('numeric')

      step.should have(1).session_variables
      step.session_variables.first.should eq('myvar3')
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

      service.steps.should have(1).item
      service.should be_valid

      step = service.steps.first
      step.name.should eq('my-step')

      step.should have(2).response_variables
      var_1, var_2 = step.response_variables

      var_1.name.should eq('myresp1')
      var_1.display_name.should eq('Response One')
      var_1.type.should eq('string')

      var_2.name.should eq('myresp2')
      var_2.display_name.should eq('Response Two')
      var_2.type.should eq('numeric')
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

      service.steps.should have(1).item
      service.should be_valid

      step = service.steps.first
      step.name.should eq('my-step')
      step.response_type.should eq('flow')
      step.should have(0).response_variables
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

      service.global_variables.should have(2).item
      service.should be_valid

      global_var_1 = service.global_variables.first
      global_var_1.name.should eq('globalvar1')
      global_var_1.display_name.should eq('Global Var One')
      global_var_1.type.should eq('string')
      global_var_1.value.should be_nil

      global_var_2 = service.global_variables.last
      global_var_2.name.should eq('globalvar2')
      global_var_2.display_name.should eq('Global Var Two')
      global_var_2.type.should eq('numeric')
      global_var_2.value.should be_nil
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

      service.steps.should have(1).item
      service.steps.first.should be_invalid
      service.should be_invalid
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

    service.name.should eq('My service')
    service.steps.should have(1).item
    service.should be_valid

    step = service.steps.first
    step.name.should eq('my-step')
    step.display_name.should eq('My step')
    step.icon.should eq('http://example.com/icon.png')
    step.kind.should eq('script')
    step.script.should eq('1')
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

      service.name.should eq('Empty service')
      service.should be_valid
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

      service.name.should eq('My service')
      service.steps.should have(1).item
      service.should be_valid

      step = service.steps.first
      step.name.should eq('my-step')
      step.display_name.should eq('My step')
      step.icon.should eq('http://example.com/icon.png')
      step.callback_url.should eq('http://example.com/callback/')
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

      service.should be_valid
      service.save!
      service.reload.steps.should have(2).items

      updated_step = service.steps.first
      updated_step.name.should eq('step_to_be_updated')
      updated_step.display_name.should eq('To be updated')
      updated_step.icon.should eq('http://example.com/icon.png')

      created_step = service.steps.last
      created_step.name.should eq('step_to_be_created')
      created_step.display_name.should eq('New step')

      service.steps.where(:id => to_be_deleted.id).should have(0).items
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

        service.global_variables.should have(1).items

        updated_global_var_1 = service.global_variables.first
        updated_global_var_1.name.should eq('globalvar1')
        updated_global_var_1.display_name.should eq('Updated Global Var One')
        updated_global_var_1.type.should eq('numeric')
        updated_global_var_1.value.should eq('global_var_1_value')
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

        service.global_variables.should have(1).items
        service.global_variables.detect{|v| v.name == 'globalvar1'}.should be_nil
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

        service.global_variables.should have(2).items

        global_var_2 = service.global_variables.detect{|v| v.name == 'globalvar2'}
        global_var_2.name.should eq('globalvar2')
        global_var_2.display_name.should eq('Global Var Two')
        global_var_2.type.should eq('numeric')
        global_var_2.value.should be_nil
      end
    end

  end
end