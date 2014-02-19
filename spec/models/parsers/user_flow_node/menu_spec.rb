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
    describe Menu do

      let(:call_flow) { CallFlow.make }
      it "should compile to a verboice equivalent flow" do
        File.stub(:exists?).and_return{true}
        menu = Menu.new call_flow, 'id' => 1,
          'type' => 'menu',
          'name' => 'Menu number one',
          'store' => 'some_variable',
          'explanation_resource' => {
            "guid" => 5
          },
          'options_resource' => {
            "guid" => 7
          },
          'invalid_resource' => {
            "guid" => 8
          },
          'timeout'=> 20,
          'number_of_attempts' => 3,
          'options' => [
            {
              'description' => 'foo',
              'number' => 4,
              'next' => 10
            },
            {
              'description' => 'bar',
              'number' => 6,
              'next' => 14
            },
            {
              'description' => 'zzz',
              'number' => 2,
              'next' => 5
            }
          ],
          'default' => 45

        play1 = Play.new call_flow, 'id' => 10,
          'type' => 'play',
          'name' => 'Play 1',
          'resource' => {
            "guid" => 1
          }
        play2 = Play.new call_flow, 'id' => 14,
          'type' => 'play',
          'name' => 'Play 2',
          'resource' => {
            "guid" => 2
          }
        play3 = Play.new call_flow, 'id' => 5,
          'type' => 'play',
          'name' => 'Play 3',
          'resource' => {
            "guid" => 3
          }
        play4 = Play.new call_flow, 'id' => 45,
            'type' => 'play',
            'name' => 'Play 45',
            'resource' => {
              "guid" => 45
            }

        menu.solve_links_with [ play1, play2, play3, play4 ]

        menu.equivalent_flow.first.should eq(
          Compiler.parse do |c|
            c.Label 1
            c.StartUserStep :menu, 1, "Menu number one"
            c.PlayResource 5
            c.AssignValue 'attempt_number1', 1
            c.While 'attempt_number1 <= 3' do |c|
              c.Capture resource: 7, finish_on_key: '', timeout: 20
              c.Assign 'value_1', 'digits'
              c.If "digits == '4'" do |c|
                c.SetStepResult :pressed, "digits"
                c.PersistVariable 'some_variable', 'value_1'
                c.Label 10
                c.StartUserStep :play, 10, "Play 1"
                c.PlayResource 1
                c.Trace call_flow_id: call_flow.id, step_id: 10, step_name: 'Play 1', store: '"Message played."'
                c.Goto "end1"
              end
              c.If "digits == '6'" do |c|
                c.SetStepResult :pressed, "digits"
                c.PersistVariable 'some_variable', 'value_1'
                c.Label 14
                c.StartUserStep :play, 14, "Play 2"
                c.PlayResource 2
                c.Trace call_flow_id: call_flow.id, step_id: 14, step_name: 'Play 2', store: '"Message played."'
                c.Goto "end1"
              end
              c.If "digits == '2'" do |c|
                c.SetStepResult :pressed, "digits"
                c.PersistVariable 'some_variable', 'value_1'
                c.Label 5
                c.StartUserStep :play, 5, "Play 3"
                c.PlayResource 3
                c.Trace call_flow_id: call_flow.id, step_id: 5, step_name: 'Play 3', store: '"Message played."'
                c.Goto "end1"
              end
              c.If "digits != null" do |c|
                c.PlayResource 8
                c.SetStepResult :invalid_key
              end
              c.Else do |c|
                c.SetStepResult :timeout
              end
              c.Assign 'attempt_number1', 'attempt_number1 + 1'
            end
            c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Menu number one', store: '"Missed input for 3 times."'
            c.PersistVariable 'some_variable', nil
            c.Label 45
            c.StartUserStep :play, 45, "Play 45"
            c.PlayResource 45
            c.Trace call_flow_id: call_flow.id, step_id: 45, step_name: 'Play 45', store: '"Message played."'
            c.Label "end1"
          end.first
        )
      end

      it "should compile to a minimum verboice equivalent flow" do
        menu = Menu.new call_flow, 'id' => 27, 'type' => 'menu'
        menu.equivalent_flow.make.should eq(
          Compiler.make do |c|
            c.StartUserStep :menu, 27, ""
            c.AssignValue 'attempt_number27', 1
            c.While 'attempt_number27 <= 3' do |c|
              c.Capture finish_on_key: '', timeout: 5
              c.Assign 'value_27', 'digits'
              c.If "digits != null" do |c|
                c.SetStepResult :invalid_key
              end
              c.Else do |c|
                c.SetStepResult :timeout
              end
              c.Assign 'attempt_number27', 'attempt_number27 + 1'
            end
            c.Trace call_flow_id: call_flow.id, step_id: 27, step_name: '', store: '"Missed input for 3 times."'
            c.Label 'end27'
          end
        )
      end

      it "should handle a menu input stream"do
        (Menu.can_handle? 'id' => 27, 'type' => 'menu').should be_true
        (Menu.can_handle? 'id' => 27, 'type' => 'answer').should be_false
      end

      it "should build with a collection of options" do
        menu = Menu.new call_flow, 'id' => 27, 'type' => 'menu',
          'options' => [
            {
              'description' => 'foo',
              'number' => 1,
              'next' => 10
            },
            {
              'description' => 'bar',
              'number' => 2,
              'next' => 14
            }
          ]
        menu.options.size.should eq(2)
        menu.options.first['number'].should eq(1)
        menu.options.first['description'].should eq('foo')
        menu.options.first['next'].should eq(10)
        menu.options.last['number'].should eq(2)
        menu.options.last['description'].should eq('bar')
        menu.options.first['next'].should eq(10)
      end

      it "should resolve it's next links from a given list of commands" do
        menu = Menu.new call_flow, 'id' => 27, 'type' => 'menu',
          'options' =>[
            {
              'description' => 'foo',
              'number' => 1,
              'next' => 10
            },
            {
              'description' =>'bar',
              'number' => 2,
              'next' => 14
            }
          ]
        menu_2 = Menu.new call_flow, 'id' => 10,
          'type' => 'menu'
        menu_3 = Menu.new call_flow, 'id' => 14,
          'type' => 'menu'

        menu.solve_links_with [ menu_2, menu_3 ]
        menu.options[0]['next'].should eq(menu_2)
        menu.options[1]['next'].should eq(menu_3)
      end

      it "should respond if it's a root or not" do
        menu_1 = Menu.new call_flow, 'id' => 10,
          'root' => 1,
          'type' => 'menu'
        menu_2 = Menu.new call_flow, 'id' => 14,
          'type' => 'menu'
        menu_1.is_root?.should be_true
        menu_2.is_root?.should be_false
      end
    end
  end
end
