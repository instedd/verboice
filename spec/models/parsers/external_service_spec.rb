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

  end


end