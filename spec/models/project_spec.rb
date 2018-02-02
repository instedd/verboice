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

describe Project do

  context "validations" do
    before(:each) { Project.make }

    it { is_expected.to belong_to(:account) }
    it { is_expected.to have_many(:call_logs) }

    it { is_expected.to validate_presence_of(:name) }
    it { is_expected.to validate_uniqueness_of(:name).scoped_to(:account_id) }
  end

  context "config" do
    include ProjectConfigHelpers

    it "should be encrypted" do
      subject.config = {:some => :config}
      expect(subject.encrypted_config).not_to eq({:some => :config})
    end

    it "should propertly saved the encrypted config to the db" do
      project = Project.make
      with_callback_url_accessors do |accessor|
        project.send("#{accessor}=", accessor.to_s)
        project.save
        expect(subject.class.find(project.id).send(accessor)).to eq(accessor.to_s)
      end
    end

    it "should have accessors for all configuration" do
      with_callback_url_accessors do |accessor|
        subject.send("#{accessor}=", accessor)
        expect(subject.send(accessor)).to eq(accessor)
      end
    end
  end

  context "variables" do
    it "should gather variables from call flows" do
      project = Project.make
      project.call_flows.make user_flow: [{
      'id' => 1,
      'root' => true,
      'type' => 'capture',
      'name' => 'Capture number one',
      'store' => 'some_variable',
      'instructions_message' => { "name" => 'First Capture', 'type' => 'text' },
      'invalid_message' => {
        "name" => "An invalid key was pressed",
        "type" => "text"
      },
      'valid_values' => '1,2-4,10-20',
      'finish_on_key' => '#',
      'min_input_length' => 1,
      'max_input_length' => 2,
      'timeout' => 10 }]

      project.call_flows.make user_flow: [{
      'id' => 1,
      'root' => true,
      'type' => 'capture',
      'name' => 'Capture number 1',
      'store' => 'foo',
      'instructions_message' => { "name" => 'First Capture', 'type' => 'text' },
      'invalid_message' => {
        "name" => "An invalid key was pressed",
        "type" => "text"
      },
      'valid_values' => '1,2-4,10-20',
      'finish_on_key' => '#',
      'min_input_length' => 1,
      'max_input_length' => 2,
      'timeout' => 10,
      'next' => 2 },

    {
      'id' => 2,
      'root' => false,
      'type' => 'capture',
      'name' => 'Capture number 2',
      'store' => 'some_variable',
      'instructions_message' => { "name" => 'First Capture', 'type' => 'text' },
      'invalid_message' => {
        "name" => "An invalid key was pressed",
        "type" => "text"
      },
      'valid_values' => '1,2-4,10-20',
      'finish_on_key' => '#',
      'min_input_length' => 1,
      'max_input_length' => 2,
      'timeout' => 10 }]

      expect(project.reload.defined_variables).to eq(['some_variable', 'foo', 'language', 'sms_number'])
    end
  end

end
