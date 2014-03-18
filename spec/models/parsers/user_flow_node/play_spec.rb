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
    describe Play do

      let(:call_flow) { CallFlow.make }

      it "should compile to a verboice equivalent flow" do
        File.stub(:exists?).and_return{true}
        play = Play.new call_flow, 'id' => 1,
          'type' => 'play',
          'name' => 'Play',
          'resource' => {
            "guid" => 5
          }

        play.equivalent_flow.first.should eq(
          Compiler.parse do |c|
            c.Label 1
            c.StartUserStep :play, 1, "Play"
            c.PlayResource 5
            c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Play', store: '"Message played."'
          end.first
        )
      end

      it "shouldn't compile the resource command if there is no resource provided" do
        play = Play.new call_flow, 'id' => 1,
          'type' => 'play',
          'name' => 'Play'

        play.equivalent_flow.first.should eq(
          Compiler.parse do |c|
            c.Label 1
            c.StartUserStep :play, 1, "Play"
            c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Play', store: '"Message played."'
          end.first
        )
      end

      it "shouldn't compile the resource command if there is no resource id provided" do
        play = Play.new call_flow, 'id' => 27,
          'type' => 'play',
          'name' => 'Play number one',
          'resource' => {}

        play.equivalent_flow.first.should eq(
          Compiler.parse do |c|
            c.Label 27
            c.StartUserStep :play, 27, "Play number one"
            c.Trace call_flow_id: call_flow.id, step_id: 27, step_name: 'Play number one', store: '"Message played."'
          end.first
        )
      end

      it "shouldn't compile the resource command if the resource id provided is nil" do
        play = Play.new call_flow, 'id' => 27,
          'type' => 'play',
          'name' => 'Play number one',
          'resource' => {
            "guid" => nil
          }

        play.equivalent_flow.first.should eq(
          Compiler.parse do |c|
            c.Label 27
            c.StartUserStep :play, 27, "Play number one"
            c.Trace call_flow_id: call_flow.id, step_id: 27, step_name: 'Play number one', store: '"Message played."'
          end.first
        )
      end
    end
  end
end
