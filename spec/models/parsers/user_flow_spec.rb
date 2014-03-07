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

describe Parsers::UserFlow do
  let(:call_flow) do
    app = mock('call_flow')
    app.stub(:id).and_return 5
    app
  end

  let (:user_flow) do
    [
      {
        'id' => 27,
        'root' => 2,
        'type' => 'play',
        'name' => 'Play number 27',
        'resource' => {
          "guid" => "resource 27 guid"
        }
      },
      {
        'id' => 3,
        'type' => 'menu',
        'name' => 'Menu number one',
        'explanation_resource' => { "guid" => "resource of menu 3" },
        'options_resource' => {},
        'invalid_resource' => {},
        'timeout' => 20,
        'options' =>[
          {
            'description' => 'foobar',
            'number' => 2,
            'next' => 4
          },
          {
            'description' => 'Some other option',
            'number' => 1,
            'next' => 6
          }
        ],
        'next' => 5
      },
      {
        'id' => 4,
        'type' => 'play',
        'name' => 'Say number 4',
        'resource' => {
          "guid" => "resource 4 guid"
        }
      },
      {
        'id' => 1,
        'root' => 1,
        'type' => 'play',
        'name' => 'Play number one',
        'resource' => {
          "guid" => "resource 1 guid"
        },
        'next' => 2
      },
      {
        'id' => 2,
        'type' => 'capture',
        'name' => 'Capture number one',
        'instructions_resource' => { "guid" => 'First Capture message guid' },
        'invalid_resource' => {
          "guid" => "resource 2 invalid guid"
        },
        'end_call_resource' => {
          "guid" => "resource 2 end call guid"
        },
        'valid_values' => '1-10',
        'finish_on_key' => '#',
        'min_input_length' => 1,
        'max_input_length' => 10,
        'timeout' => 10,
        'next' => 3
      },
      {
        'id' => 6,
        'type' => 'play',
        'name' => 'Say number 6',
        'resource' => {
          "guid" => "resource 6 guid"
        }
      },
      {
        'id' => 5,
        'type' => 'play',
        'name' => 'Say number 5',
        'resource' => {
          "guid" => "resource 5 guid"
        },
        'next' => 7
      },
      {
        'id' => 7,
        'type' => 'goto',
        'jump' => 33
      },
      {
        'id' => 33,
        'root' => 3,
        'type' => 'play',
        'name' => 'Play number 33',
        'resource' => {
          "guid" => "resource 33 guid"
        },
        'next' => 34
      },
      {
        'id' => 34,
        'type' => 'branch',
        'name' => 'Branch number one',
        'options' => [
          {
            'description' => 'foo',
            'conditions' => [
              {
                'step' => 3,
                'operator' => '==',
                'value' => 6
              },
              {
                'step' => 2,
                'operator' => '<',
                'value' => 30
              },
              {
                'step' => 2,
                'operator' => '>=',
                'value' => 5
              }
            ],
            'next' => 10
          },
          {
            'description' => 'bar',
            'conditions' => [
              {
                'step' => 3,
                'operator' => '<=',
                'value' => 5
              }
            ],
            'next' => 14
          }
        ]
      },
      {
        'id' => 10,
        'type' => 'play',
        'name' => 'Play number 10',
        'resource' => {
          "guid" => "resource 10 guid"
        }
      },
      {
        'id' => 14,
        'type' => 'play',
        'name' => 'Say 14',
        'resource' => {
          "guid" => "resource 14 guid"
        },
        'next' => 15
      },
      {
        'id' => 15,
        'name' => 'Hanged up!',
        'type' => 'hang_up'
      },
      {
        'id' => 44,
        'root' => 4,
        'type' => 'play',
        'name' => 'Play number 44',
        'resource' => {
          "guid" => "resource 44 guid"
        }
      }
    ]
  end

  it "should retrieve an equivalent flow in verboice internal representation" do
    File.stub(:exists?).and_return{true}
    (Parsers::UserFlow.new call_flow, user_flow).equivalent_flow.should eq(
      Compiler.make do
        Answer()
        StartUserStep :play, 1, "Play number one"
        PlayResource "resource 1 guid"
        Trace call_flow_id: 5, step_id: 1, step_name: 'Play number one', store: '"Message played."'
        StartUserStep :input, 2, "Capture number one"
        AssignValue 'attempt_number2', 1
        While 'attempt_number2 <= 3' do
          Capture resource: "First Capture message guid", min: 1, max: 10, finish_on_key: '#', timeout: 10
          Assign 'value_2', 'digits'
          If "(digits >= 1 && digits <= 10)" do
            SetStepResult :pressed, "digits"
            Goto "end2"
          end
          If "digits != null" do
            SetStepResult :invalid_key
            Trace call_flow_id: 5, step_id: 2, step_name: 'Capture number one', store:  '"Invalid key pressed"'
            PlayResource "resource 2 invalid guid"
          end
          Else do
            SetStepResult :timeout
          end
          Assign 'attempt_number2', 'attempt_number2 + 1'
        end
        Trace call_flow_id: 5, step_id: 2, step_name: 'Capture number one', store: '"Missed input for 3 times."'
        Label "end2"
        StartUserStep :menu, 3, "Menu number one"
        PlayResource "resource of menu 3"
        AssignValue 'attempt_number3', 1
        While 'attempt_number3 <= 3' do
          Capture finish_on_key: '', timeout: 20
          Assign 'value_3', 'digits'
          If "digits == '2'" do
            SetStepResult :pressed, "digits"
            StartUserStep :play, 4, "Say number 4"
            PlayResource "resource 4 guid"
            Trace call_flow_id: 5, step_id: 4, step_name: 'Say number 4', store: '"Message played."'
            Goto "end3"
          end
          If "digits == '1'" do
            SetStepResult :pressed, "digits"
            StartUserStep :play, 6, "Say number 6"
            PlayResource "resource 6 guid"
            Trace call_flow_id: 5, step_id: 6, step_name: 'Say number 6', store: '"Message played."'
            Goto "end3"
          end
          If "digits != null" do
            SetStepResult :invalid_key
          end
          Else do
            SetStepResult :timeout
          end
          Assign 'attempt_number3', 'attempt_number3 + 1'
        end
        Trace call_flow_id: 5, step_id: 3, step_name: 'Menu number one', store: '"Missed input for 3 times."'
        Label "end3"
        StartUserStep :play, 5, "Say number 5"
        PlayResource "resource 5 guid"
        Trace call_flow_id: 5, step_id: 5, step_name: 'Say number 5', store: '"Message played."'
        StartUserStep :play, 33, "Play number 33"
        PlayResource "resource 33 guid"
        Trace call_flow_id: 5, step_id: 33, step_name: 'Play number 33', store: '"Message played."'
        StartUserStep :branch, 34, "Branch number one"
        If "(typeof(value_3) != 'undefined' && typeof(6) != 'undefined' && value_3 == 6) && (typeof(value_2) != 'undefined' && typeof(30) != 'undefined' && value_2 < 30) && (typeof(value_2) != 'undefined' && typeof(5) != 'undefined' && value_2 >= 5)" do
          Trace call_flow_id: 5, step_id: 34, step_name: 'Branch number one', store: '"Branch number 1 selected: \'Play number 10\'"'
          SetStepResult :selected, "1"
          Label 10
          StartUserStep :play, 10, "Play number 10"
          PlayResource "resource 10 guid"
          Trace call_flow_id: 5, step_id: 10, step_name: 'Play number 10', store: '"Message played."'
          Goto "end34"
        end
        If "(typeof(value_3) != 'undefined' && typeof(5) != 'undefined' && value_3 <= 5)" do
          Trace call_flow_id: 5, step_id: 34, step_name: 'Branch number one', store: '"Branch number 2 selected: \'Say 14\'"'
          SetStepResult :selected, "2"
          Label 14
          StartUserStep :play, 14, "Say 14"
          PlayResource "resource 14 guid"
          Trace call_flow_id: 5, step_id: 14, step_name: 'Say 14', store: '"Message played."'
          Label 15
          Trace call_flow_id: 5, step_id: 15, step_name: 'Hanged up!', store: '"Verboice ended call."'
          End()
          Goto "end34"
        end
        Trace(call_flow_id: 5, step_id: 34, step_name: 'Branch number one', store: '"No branch was selected."')
        SetStepResult :no_branch
        Label "end34"
      end
    )
  end

  it "should provide a hash of step names and IDs" do
    (Parsers::UserFlow.new call_flow, user_flow).step_names.should eq({
      27 => "Play number 27",
      3  => 'Menu number one',
      4  => 'Say number 4',
      1  => "Play number one",
      2  => "Capture number one",
      6  => "Say number 6",
      5  => "Say number 5",
      33 => "Play number 33",
      34 => "Branch number one",
      10 => "Play number 10",
      14 => "Say 14",
      15 => "Hanged up!",
      44 => "Play number 44"
    })
  end

end
