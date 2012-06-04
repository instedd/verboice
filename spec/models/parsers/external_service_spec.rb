require 'spec_helper'

describe Parsers::ExternalService do

  def parse(xml)
    @service = Parsers::ExternalService.new(@existing_service).parse(xml)
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
              callback-url="http://example.com/callback/"
              response-type="variables">
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
      step.response_type.should eq('variables')
      step.callback_url.should eq('http://example.com/callback/')
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
              callback-url="http://example.com/callback/"
              response-type="variables">
              <settings>
                <variable name="my-var-1" display-name="Variable One" type="string"/>
                <variable name="my-var-2" display-name="Variable Two" type="numeric"/>
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

      var_1.name.should eq('my-var-1')
      var_1.display_name.should eq('Variable One')
      var_1.type.should eq('string')

      var_2.name.should eq('my-var-2')
      var_2.display_name.should eq('Variable Two')
      var_2.type.should eq('numeric')
    end

    it "should create a new external service with a callback step with several response variables" do
        parse <<-XML
          <verboice-service>
            <name>My service</name>
            <steps>
              <step name="my-step"
                display-name="My step"
                icon="http://example.com/icon.png"
                type="callback"
                callback-url="http://example.com/callback/"
                response-type="variables">
                <settings>
                  <variable name="my-var-1" display-name="Variable One" type="string"/>
                  <response-variable name="my-resp-1" display-name="Response One" type="string"/>
                  <response-variable name="my-resp-2" display-name="Response Two" type="numeric"/>
                </settings>
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

        var_1.name.should eq('my-resp-1')
        var_1.display_name.should eq('Response One')
        var_1.type.should eq('string')

        var_2.name.should eq('my-resp-2')
        var_2.display_name.should eq('Response Two')
        var_2.type.should eq('numeric')
      end


    it "should create a new external service with global settings" do
      parse <<-XML
        <verboice-service>
          <name>My Service</name>
          <global-settings>
            <variable name="global-var-1" display-name="Global Var One" type="string"/>
            <variable name="global-var-2" display-name="Global Var Two" type="numeric"/>
          </global-settings>
        </verboice-service>
      XML

      service.global_variables.should have(2).item
      service.should be_valid

      global_var_1 = service.global_variables.first
      global_var_1.name.should eq('global-var-1')
      global_var_1.display_name.should eq('Global Var One')
      global_var_1.type.should eq('string')
      global_var_1.value.should be_nil

      global_var_2 = service.global_variables.last
      global_var_2.name.should eq('global-var-2')
      global_var_2.display_name.should eq('Global Var Two')
      global_var_2.type.should eq('numeric')
      global_var_2.value.should be_nil
    end

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
              callback-url="http://example.com/callback/"
              response-type="variables">
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
          v.name = 'global-var-1'
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
              <variable name="global-var-1" display-name="Updated Global Var One" type="numeric"/>
            </global-settings>
          </verboice-service>
        XML

        service.global_variables.should have(1).items

        updated_global_var_1 = service.global_variables.first
        updated_global_var_1.name.should eq('global-var-1')
        updated_global_var_1.display_name.should eq('Updated Global Var One')
        updated_global_var_1.type.should eq('numeric')
        updated_global_var_1.value.should eq('global_var_1_value')
      end

      it "should delete removed variables" do
        parse <<-XML
          <verboice-service>
            <name>My Service</name>
            <global-settings>
              <variable name="global-var-2" display-name="Global Var Two" type="numeric"/>
            </global-settings>
          </verboice-service>
        XML

        service.global_variables.should have(1).items
        service.global_variables.detect{|v| v.name == 'global-var-1'}.should be_nil
      end

      it "should add new variables" do
        parse <<-XML
          <verboice-service>
            <name>My Service</name>
            <global-settings>
              <variable name="global-var-1" display-name="Global Var One" type="string"/>
              <variable name="global-var-2" display-name="Global Var Two" type="numeric"/>
            </global-settings>
          </verboice-service>
        XML

        service.global_variables.should have(2).items

        global_var_2 = service.global_variables.detect{|v| v.name == 'global-var-2'}
        global_var_2.name.should eq('global-var-2')
        global_var_2.display_name.should eq('Global Var Two')
        global_var_2.type.should eq('numeric')
        global_var_2.value.should be_nil
      end
    end

  end
end