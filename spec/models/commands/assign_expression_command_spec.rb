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

describe Commands::AssignExpressionCommand do

  let(:session) { Session.new pbx: double('pbx'), call_log: CallLog.make}

  it "assigns" do
    cmd = Commands::AssignExpressionCommand.new 'foo', '1 + 2'
    cmd.next = :next
    cmd.run(session).should eq(:next)

    session['foo'].should eq(3)
  end

  it "assigns raising exception if invalid" do
    expect {
      cmd = Commands::AssignExpressionCommand.new 'foo', 'invalid'
      cmd.next = :next
      cmd.run(session)
    }.to raise_error

    session['foo'].should be_nil
  end

  it "assigns without raising exception" do
    cmd = Commands::AssignExpressionCommand.new 'foo', 'invalid', :try
    cmd.next = :next
    cmd.run(session).should eq(:next)

    session['foo'].should be_nil
  end
end
