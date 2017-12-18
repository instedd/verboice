# Copyright (C) 2010-2017, InSTEDD
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

describe FlowResults::Question do
  it "maps a capture to a FLOIP numeric question" do
    # TODO: this test is isolated but if the representation
    # of steps in call flow changes it will continue to
    # pass happily. We should test this at a "coarser"
    # level.
    # Ideally there would be some SOT for this structure
    # but I can't seem to find it.
    step = {
      'id' => 1,
      'root' => true,
      'type' => 'capture',
      'name' => 'Capture number one',
      'store' => 'some_variable',
      'instructions_message' => { "name" => 'First Capture', 'type' => 'text' },
      'invalid_message' => {
        "name" => "An invalid key was pressed",
        "type" => "recording",
        "file" => "file.wav",
        "duration" => 5
      },
      'valid_values' => '1,2-4,10-20',
      'finish_on_key' => '#',
      'min_input_length' => 1,
      'max_input_length' => 2,
      'timeout' => 10 }

    q = FlowResults::Question.from_step(step)
    q.should be_a(FlowResults::Question::Numeric)
    q.type.should eq(:numeric)
    q.label.should eq("Capture number one")
  end

  it "maps a menu to a FLOIP select one question" do
    step = {
      "id"=>1,
      "name"=>"Initial menu",
      "type"=>"menu",
      "root"=>true,
      "options"=>[{"number"=>1, "next"=>593}, {"number"=>2, "next"=>737}],
      "end_call_message"=>{"name"=>"Bye", "type"=>"recording", "duration"=>"00:00"},
      "invalid_message"=>
      {"name"=>"Wrong number!", "type"=>"recording", "duration"=>"00:00"},
      "explanation_message"=>
      {"name"=>"Welcome to test call_flow 01",
        "type"=>"recording",
        "duration"=>"00:00"},
      "options_message"=>
      {"name"=>"Press 1 for foo, press 2 for bar",
        "type"=>"recording",
        "duration"=>"00:00"}
    }

    q = FlowResults::Question.from_step(step)
    q.should be_a(FlowResults::Question::SelectOne)
    q.type.should eq(:select_one)
    q.label.should eq("Initial menu")
    q.choices.should eq(["1", "2"])
  end

  it "maps non-capturing step types to nil" do
    step = { "type" => "an_unknown_type" }
    q = FlowResults::Question.from_step(step)
    q.should be_nil
  end
end
