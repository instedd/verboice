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

module Commands
  describe WhileCommand do
    let(:session) { Session.new pbx: double('pbx'), call_log: CallLog.make}

    it "while when true" do
      session['i'] = 0

      cmd = WhileCommand.new 'i == 0', AssignExpressionCommand.new(:i, 'i + 1')
      result = cmd.run(session)
      result.should be_instance_of(Commands::AssignExpressionCommand)
      result.next.should be(cmd)
    end

    it "while when false" do
      session['i'] = 0

      cmd = WhileCommand.new 'i != 0', AssignExpressionCommand.new(:i, 'i + 1')
      cmd.next = :next
      cmd.run(session).should == :next
    end
  end
end