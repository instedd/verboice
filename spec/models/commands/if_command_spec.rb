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
  describe IfCommand do
    let(:session) { Session.new pbx: double('pbx'), call_log: CallLog.make}

    it "if variable true branch" do
      session[:some_var] = true

      cmd = IfCommand.new :some_var, :first, :second
      cmd.run(session).should == :first
    end

    it "if variable else branch" do
      session[:some_var] = false

      cmd = IfCommand.new :some_var, :first, :second
      cmd.run(session).should == :second
    end

    it "if variable true branch empty" do
      session[:some_var] = true

      cmd = IfCommand.new :some_var, nil
      cmd.next = :next
      cmd.run(session).should == :next
    end

    it "if variable else branch empty" do
      session[:some_var] = false

      cmd = IfCommand.new :some_var, nil
      cmd.next = :next
      cmd.run(session).should == :next
    end

    it "if variable with complex condition" do
      session[:var1] = 1
      session[:var2] = 2

      cmd = IfCommand.new "var1 + var2 == 3", :first
      cmd.run(session).should == :first
    end
  end
end