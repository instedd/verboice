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

module Parsers
  module UserFlowNode
    describe Capture do

      let(:call_flow) { CallFlow.make }

      it "should compile to a verboice equivalent flow" do
        File.stub(:exists?).and_return{true}
        capture = Capture.new call_flow,
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
          'timeout' => 10

        capture.equivalent_flow.first.should eq(
          Compiler.parse do |c|
            c.Label 1
            c.Assign "current_step", 1
            c.AssignValue "current_step_name", "Capture number one"
            c.Assign 'attempt_number1', '1'
            c.While 'attempt_number1 <= 3' do |c|
              c.Capture say: "First Capture", min: 1, max: 2, finish_on_key: '#', timeout: 10
              c.Assign 'value_1', 'digits'
              c.PersistVariable 'some_variable', 'value_1'
              c.If "(digits == 1) || (digits >= 2 && digits <= 4) || (digits >= 10 && digits <= 20)" do |c|
                c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Capture number one', store: '"User pressed: " + (digits ? digits : "<empty>")'
                c.Goto "end1"
              end
              c.If "digits != null" do |c|
                c.PlayFile "1-invalid"
                c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Capture number one', store: '"Invalid key pressed"'
              end
              c.Else do |c|
                c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Capture number one', store: '"No key was pressed. Timeout."'
              end
              c.Assign 'attempt_number1', 'attempt_number1 + 1'
            end
            c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Capture number one', store: '"Missed input for 3 times."'
            c.Label "end1"
          end.first
        )
      end

      it "should accept an empty 'valid_values' string and use it as 'all values are valid'" do

        capture_flow = Compiler.parse do |c|
          c.Label 4
          c.Assign "current_step", 4
          c.AssignValue "current_step_name", "Capture"
          c.Assign 'attempt_number4', '1'
          c.While 'attempt_number4 <= 3' do |c|
            c.Capture min: 1, max: 1, finish_on_key: '#', timeout: 5
            c.Assign 'value_4', 'digits'
            c.If 'digits != null' do |c|
              c.Trace call_flow_id: call_flow.id, step_id: 4, step_name: 'Capture', store: '"User pressed: " + (digits ? digits : "<empty>")'
              c.Goto "end4"
            end
            c.Else do |c|
              c.Trace call_flow_id: call_flow.id, step_id: 4, step_name: 'Capture', store: '"No key was pressed. Timeout."'
            end
            c.Assign 'attempt_number4', 'attempt_number4 + 1'
          end
          c.Trace call_flow_id: call_flow.id, step_id: 4, step_name: 'Capture', store: '"Missed input for 3 times."'
          c.Label "end4"
        end.first

        capture = Capture.new call_flow,
          'id' => 4,
          'root' => true,
          'type' => 'capture',
          'name' => 'Capture'

        capture.equivalent_flow.first.should eq(capture_flow)

        capture = Capture.new call_flow,
          'id' => 4,
          'root' => true,
          'type' => 'capture',
          'name' => 'Capture',
          'valid_values' => ''

        capture.equivalent_flow.first.should eq(capture_flow)

        capture = Capture.new call_flow,
          'id' => 4,
          'root' => true,
          'type' => 'capture',
          'name' => 'Capture',
          'valid_values' => '   '

        capture.equivalent_flow.first.should eq(capture_flow)
      end

      it "should accept an empty input" do
        File.stub(:exists?).and_return{true}
        capture_flow = Compiler.parse do |c|
          c.Label 4
          c.Assign "current_step", 4
          c.AssignValue "current_step_name", "Capture"
          c.Assign 'attempt_number4', '1'
          c.While 'attempt_number4 <= 3' do |c|
            c.Capture min: 0, max: 2, finish_on_key: '#', timeout: 5
            c.Assign 'value_4', 'digits'
            c.If '(digits == 1) || (digits >= 2 && digits <= 4) || (digits >= 10 && digits <= 20) || (digits == null)' do |c|
              c.Trace call_flow_id: call_flow.id, step_id: 4, step_name: 'Capture', store: '"User pressed: " + (digits ? digits : "<empty>")'
              c.Goto "end4"
            end
            c.Else do |c|
              c.PlayFile "4-invalid"
              c.Trace call_flow_id: call_flow.id, step_id: 4, step_name: 'Capture', store: '"Invalid key pressed"'
            end
            c.Assign 'attempt_number4', 'attempt_number4 + 1'
          end
          c.Trace call_flow_id: call_flow.id, step_id: 4, step_name: 'Capture', store: '"Missed input for 3 times."'
          c.Label "end4"
        end.first

        capture = Capture.new call_flow,
          'id' => 4,
          'root' => true,
          'type' => 'capture',
          'name' => 'Capture',
          'valid_values' => '1,2-4,10-20',
          'finish_on_key' => '#',
          'min_input_length' => 0,
          'max_input_length' => 2,
          'invalid_message' => {
            "name" => "An invalid key was pressed",
            "type" => "recording",
            "file" => "file.wav",
            "duration" => 5
          }

        capture.equivalent_flow.first.should eq(capture_flow)
      end

      it "should accept an empty input for all values" do
        File.stub(:exists?).and_return{true}
        capture_flow = Compiler.parse do |c|
          c.Label 4
          c.Assign "current_step", 4
          c.AssignValue "current_step_name", "Capture"
          c.Assign 'attempt_number4', '1'
          c.While 'attempt_number4 <= 3' do |c|
            c.Capture min: 0, max: 1, finish_on_key: '#', timeout: 5
            c.Assign 'value_4', 'digits'
            c.If 'true' do |c|
              c.Trace call_flow_id: call_flow.id, step_id: 4, step_name: 'Capture', store: '"User pressed: " + (digits ? digits : "<empty>")'
              c.Goto "end4"
            end
            c.Else do |c|
              c.Trace call_flow_id: call_flow.id, step_id: 4, step_name: 'Capture', store: '"Invalid key pressed"'
            end
            c.Assign 'attempt_number4', 'attempt_number4 + 1'
          end
          c.Trace call_flow_id: call_flow.id, step_id: 4, step_name: 'Capture', store: '"Missed input for 3 times."'
          c.Label "end4"
        end.first

        capture = Capture.new call_flow,
          'id' => 4,
          'root' => true,
          'type' => 'capture',
          'name' => 'Capture',
          'min_input_length' => 0

        capture.equivalent_flow.first.should eq(capture_flow)
      end

      it "should have a default next step" do
        File.stub(:exists?).and_return{true}
        capture_flow = Compiler.parse do |c|
            c.Label 4
            c.Assign "current_step", 4
            c.AssignValue "current_step_name", "Capture"
            c.Assign 'attempt_number4', '1'
            c.While 'attempt_number4 <= 3' do |c|
              c.Capture min: 0, max: 2, finish_on_key: '#', timeout: 5
              c.Assign 'value_4', 'digits'
              c.If '(digits == 1) || (digits >= 2 && digits <= 4) || (digits >= 10 && digits <= 20) || (digits == null)' do |c|
                c.Trace call_flow_id: call_flow.id, step_id: 4, step_name: 'Capture', store: '"User pressed: " + (digits ? digits : "<empty>")'
                c.Goto "end4"
              end
              c.Else do |c|
                c.PlayFile "4-invalid"
                c.Trace call_flow_id: call_flow.id, step_id: 4, step_name: 'Capture', store: '"Invalid key pressed"'
              end
              c.Assign 'attempt_number4', 'attempt_number4 + 1'
            end
            c.Trace call_flow_id: call_flow.id, step_id: 4, step_name: 'Capture', store: '"Missed input for 3 times."'
            c.Label 2
            c.Assign "current_step", 2
            c.AssignValue "current_step_name", "Play"
            c.Trace call_flow_id: call_flow.id, step_id: 2, step_name: 'Play', store: '"Message played."'
            c.Say "Some explanation message"
            c.Label "end4"
          end.first

          capture = Capture.new call_flow,
            'id' => 4,
            'root' => true,
            'type' => 'capture',
            'name' => 'Capture',
            'valid_values' => '1,2-4,10-20',
            'finish_on_key' => '#',
            'min_input_length' => 0,
            'max_input_length' => 2,
            'invalid_message' => {
              "name" => "An invalid key was pressed",
              "type" => "recording",
              "file" => "file.wav",
              "duration" => 5
            },
            'default' => 2

          play = Play.new call_flow, 'id' => 2,
            'type' => 'play',
            'name' => 'Play',
            'message' => {
              "name" => "Some explanation message",
              "type" => "text"
            }

          capture.solve_links_with [ play ]

          capture.equivalent_flow.first.should eq(capture_flow)
      end
    end
  end
end
