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
    describe Nuntium do

      let(:call_flow) { CallFlow.make }

      before :each do
        account = double('account')
        nuntium_channels = double('nuntium_channels')
        channel = double('channel', id: 7)
        expect(call_flow).to receive(:account).and_return(account)
        expect(account).to receive(:nuntium_channels).and_return(nuntium_channels)
        expect(nuntium_channels).to receive(:find_by_id).with(7).and_return(channel)
      end

      it "should compile to a verboice equivalent flow" do
        allow(File).to receive(:exists?).and_return(true)
        nuntium = Nuntium.new call_flow, 'id' => 1,
          'type' => 'nuntium',
          'name' => 'Nuntium',
          'resource' => {
            "guid" => 5
          },
          'recipient' => {
            'caller' => true
          },
          'channel_id' => 7

        expect(nuntium.equivalent_flow.first).to eq(
          Compiler.parse do |c|
            c.Label 1
            c.StartUserStep :nuntium, 1, "Nuntium"
            c.Nuntium 5, 7, :caller
            c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Nuntium', store: '"Sent text message."'
          end.first
        )
      end

      it "shouldn't compile the nuntium command if no resource is given" do
        allow(File).to receive(:exists?).and_return(true)
        nuntium = Nuntium.new call_flow, 'id' => 1,
          'type' => 'nuntium',
          'name' => 'Nuntium',
          'resource' => {
            "guid" => nil
          },
          'recipient' => {
            'caller' => true
          },
          'channel_id' => 7

        expect(nuntium.equivalent_flow.first).to eq(
          Compiler.parse do |c|
            c.Label 1
            c.StartUserStep :nuntium, 1, "Nuntium"
            c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Nuntium', store: '"Sent text message."'
          end.first
        )
      end

      [[{ 'value' => '555-1111' }, "'555-1111'"],
       [{ 'step' => 20 }, 'value_20'],
       [{ 'variable' => 'foo' }, 'var_foo'],
       [{ 'response' => 20 }, 'external_20']].each do |recipient, expr|
        it "should compile the nuntium command with a value recipient" do
          allow(File).to receive(:exists?).and_return(true)
          nuntium = Nuntium.new call_flow, 'id' => 1,
            'type' => 'nuntium',
            'name' => 'Nuntium',
            'resource' => { "guid" => 42 },
            'recipient' => recipient,
            'channel_id' => 7

          expect(nuntium.equivalent_flow.first).to eq(
            Compiler.parse do |c|
              c.Label 1
              c.StartUserStep :nuntium, 1, "Nuntium"
              c.Nuntium 42, 7, :expr, expr
              c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Nuntium', store: '"Sent text message."'
            end.first
          )
        end
      end

    end
  end
end
