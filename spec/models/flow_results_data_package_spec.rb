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
require 'json-schema'

describe FlowResultsDataPackage do
  describe "relationship with CallFlow" do
    let(:call_flow) do
      Timecop.freeze Date.new(2017, 12, 1)
      cf = CallFlow.make :name => "Flow", :mode => :flow
      Timecop.return
      cf
    end

    subject { call_flow.current_data_package }

    it "creates a flow results data package if it is new" do
      call_flow.flow_results_data_packages.length.should eq(1)

      subject.uuid.should_not be_nil
      subject.call_flow.should eq(call_flow)
      subject.from.should eq(Date.new(2017, 12, 1))
      subject.until.should be_nil
    end

    it "loads an existing data package if available for call flow" do
      reloaded_call_flow = CallFlow.find(call_flow.id)
      # I just want to ensure that loading new CallFlow instances from existing
      # CallFlow database records doesn't end up creating a new data package each time
      reloaded_call_flow.flow_results_data_packages.length.should eq(1)

      loaded_package = reloaded_call_flow.current_data_package
      loaded_package.id.should eq(subject.id)
      loaded_package.uuid.should eq(subject.uuid)
    end

    it "creates new package if CallFlow flow changed" do
      old_data_package = subject

      call_flow.user_flow = [
        {
          'id' => 1,
          'root' => 1,
          'type' => 'play',
          'name' => 'Play number one',
          'resource' => {
            "guid" => "foo"
          }
        }
      ]
      call_flow.save!

      new_package = CallFlow.find(call_flow.id).current_data_package

      # Since we modified the CallFlow,
      # I want to ensure to deprecate the original datapackage
      new_package.id.should_not eq(old_data_package.id)
      new_package.uuid.should_not eq(old_data_package.uuid)

      # Here we ensure data packages coverage
      new_package.from.should eq(FlowResultsDataPackage.find(old_data_package.id).until)

      # This is another invariant: the current data package has no end of validity period
      new_package.until.should be_nil
    end

    # CallFlows managed by a third party app are opaque for Verboice and as
    # such they don't get exposed through the FLOIP API
    it "has no current data package if it is set to callback mode" do
      call_flow.mode = :callback_url
      call_flow.save!

      call_flow.flow_results_data_packages.length.should eq(1)

      subject.should be_nil
    end
  end

  describe("relationship with callback mode CallFlows") do
    let(:call_flow) do
      Timecop.freeze Date.new(2017, 12, 1)
      cf = CallFlow.make :name => "Flow", :mode => :callback_url
      Timecop.return
      cf
    end

    subject { call_flow }

    it "has no data packages" do
      subject.current_data_package.should be_nil
      subject.flow_results_data_packages.length.should eq(0)
    end

    it "starts up a data package if it changes to flow mode" do
      subject.mode = :flow
      subject.save!

      subject.current_data_package.should_not be_nil
      subject.flow_results_data_packages.length.should eq(1)
    end
  end

  describe("get descriptor for one package") do
    describe("happy path") do
      it "returns a valid data package descriptor" do
        call_flow = CallFlow.make :name => "Flow", :mode => :flow
        call_flow.user_flow = [
          {
            'id' => 1,
            'root' => 1,
            'type' => 'play',
            'name' => 'Play number one',
            'resource' => {
              "guid" => "foo"
            }
          }
        ]
        call_flow.save!

        data_package = call_flow.current_data_package

        json = JSON.parse data_package.descriptor("http://foo.com").to_json

        schema = File.join(Rails.root, 'spec/fixtures/data_package_schema.json')
        JSON::Validator.validate!(schema, json)

        json["profile"].should eq("flow-results-package")
        json["name"].should eq(data_package.name)
        json["flow-results-specification"].should eq("1.0.0-rc1")
        json["resources"][0]["path"].should eq("http://foo.com/responses")
      end
    end
  end

  describe("#floip_schema") do
    it "must contain fields attribute" do
      call_flow = CallFlow.make :name => "Flow", :mode => :flow
      call_flow.user_flow = [
        {
          'id' => 1,
          'root' => 1,
          'type' => 'play',
          'name' => 'Play number one',
          'resource' => {
            "guid" => "foo"
          }
        }
      ]
      call_flow.save!

      data_package = call_flow.current_data_package

      json = JSON.parse data_package.floip_schema.to_json

      json["fields"].should_not be_nil
    end

    it "must include questions" do
      call_flow = CallFlow.make :name => "Flow", :mode => :flow
      call_flow.user_flow = [
        {
          'id' => 1,
          'type' => 'capture',
          'name' => 'Capture sth',
        },
        {
          'id' => 2,
          'type' => 'menu',
          'name' => 'Menu sth',
          "options"=>[{"number"=>4, "next"=>1}, {"number"=>5, "next"=>1}],
        }
      ]
      call_flow.save!

      data_package = call_flow.current_data_package
      json = JSON.parse data_package.floip_schema.to_json

      json["fields"].should_not be_nil
      json["questions"].should eq({
        "1" => {
          "type" => "numeric",
          "label" => "Capture sth"
        },
        "2" => {
          "type" => "select_one",
          "label" => "Menu sth",
          "choices" => ["4", "5"]
        }
      })
    end
  end

  # TODO: language, record, write_variable
  describe("#questions") do
    it "prunes non-collection steps" do
      call_flow = CallFlow.make :name => "Flow", :mode => :flow
      call_flow.user_flow = [
        { 'id' => 1, 'type' => 'branch' },
        { 'id' => 2, 'type' => 'external' },
        { 'id' => 3, 'type' => 'goto' },
        { 'id' => 4, 'type' => 'hang_up_and_call_back' },
        { 'id' => 5, 'type' => 'hang_up' },
        { 'id' => 6, 'type' => 'mark_as_failed' },
        { 'id' => 7, 'type' => 'mark_as_successful' },
        { 'id' => 8, 'type' => 'nuntium' },
        { 'id' => 9, 'type' => 'play' },
        { 'id' => 10, 'type' => 'transfer' }
      ]
      call_flow.save!

      data_package = call_flow.current_data_package
      data_package.questions.should eq([])
    end

    it "maps capture step to FLOIP numeric question" do
      call_flow = CallFlow.make :name => "Flow", :mode => :flow
      call_flow.user_flow = [
        { 'id' => 1, 'type' => 'capture', 'name' => 'Age' }
      ]
      call_flow.save!

      data_package = call_flow.current_data_package
      data_package.questions.should eq([FlowResults::Question::Numeric.new(1, 'Age')])
    end

    it "maps menu step to FLOIP select one question" do
      call_flow = CallFlow.make :name => "Flow", :mode => :flow
      call_flow.user_flow = [
        {
          'id' => 1,
          'type' => 'menu',
          'name' => 'Favorite icecream flavor',
          'options' =>[
            {
              'number' => 1,
              'next' => 1
            },
            {
              'number' => 2,
              'next' => 1
            },
            {
              'number' => 3,
              'next' => 1
            }
          ]
        }
      ]
      call_flow.save!

      data_package = call_flow.current_data_package
      data_package.questions.should eq([FlowResults::Question::SelectOne.new(1, 'Favorite icecream flavor', ["1","2","3"])])
    end

    it "indexes questions by id" do
      questions = [
        FlowResults::Question::Numeric.new("23", "Foo"),
        FlowResults::Question::SelectOne.new("42", "Bar", ["6", "7", "8"]),
        FlowResults::Question::Numeric.new("84", "Baz")
      ]
      FlowResultsDataPackage.schema_questions(questions).keys.should eq(["23", "42", "84"])
    end
  end
end
