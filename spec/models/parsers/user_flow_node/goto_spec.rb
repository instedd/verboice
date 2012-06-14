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
    describe Goto do

      let(:call_flow) { self }

      it "should compile to a verboice equivalent flow" do
        play = Goto.new call_flow, 'id' => 1,
          'type' => 'goto',
          'jump' => 3


        play.equivalent_flow.first.should eq(
          Compiler.parse do
            Goto 3
          end.first
        )
      end
      def id
        1
      end
    end
  end
end