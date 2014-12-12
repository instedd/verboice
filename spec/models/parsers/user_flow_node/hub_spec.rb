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
    describe Hub do
      it "should build bindings js with one binding" do
        hub = Hub.new nil, "bindings"=>
           [{"name"=>"foo",
             "label"=>"Foo",
             "value"=>{"step"=>nil, "variable"=>nil, "value"=>"FOO", "response"=>nil},
             "bindings"=>[]},
            {"name"=>"bar",
             "label"=>"Bar",
             "value"=>{"step"=>nil, "variable"=>nil, "value"=>"BAR", "response"=>nil},
             "bindings"=>[]}]

        hub.bindings_js.should eq(
          "hub_payload = {};" \
          "hub_payload['foo'] = 'FOO';" \
          "hub_payload['bar'] = 'BAR';" \
        )
      end

      it "should build bindings js with nested binding" do
        hub = Hub.new nil, "bindings"=>
           [{"name"=>"properties",
             "label"=>"properties",
             "bindings"=>
              [{"name"=>"Name",
                "label"=>"Name",
                "value"=>{"step"=>nil, "variable"=>nil, "value"=>"A", "response"=>nil},
                "bindings"=>[]},
               {"name"=>"Age",
                "label"=>"Age",
                "value"=>{"step"=>nil, "variable"=>nil, "value"=>"B", "response"=>nil},
                "bindings"=>[]}]}]

        hub.bindings_js.should eq(
          "hub_payload = {};" \
          "hub_payload['properties'] = {};" \
          "hub_payload['properties']['Name'] = 'A';" \
          "hub_payload['properties']['Age'] = 'B';" \
        )
      end

      it "should build bindings js with one empty binding" do
        hub = Hub.new nil, "bindings"=>
           [{"name"=>"foo",
             "label"=>"Foo",
             "value"=>{"step"=>nil, "variable"=>nil, "value"=>"FOO", "response"=>nil},
             "bindings"=>[]},
            {"name"=>"bar",
             "label"=>"Bar",
             "value"=>{"step"=>nil, "variable"=>nil, "value"=>"", "response"=>nil},
             "bindings"=>[]}]

        hub.bindings_js.should eq(
          "hub_payload = {};" \
          "hub_payload['foo'] = 'FOO';" \
        )
      end
    end
  end
end
