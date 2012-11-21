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
  describe SayCommand do

    let(:session) { Session.new pbx: double('pbx'), call_log: CallLog.make}

    it "runs" do
      session.pbx.should_receive(:say).with('some text', anything)
      Commands::SayCommand.new('some text').run session
    end

    it "runs with interpolated string" do
      session['var_foo'] = 'world'
      session.pbx.should_receive(:say).with('hello world', anything)
      Commands::SayCommand.new('hello {foo}').run session
    end
  end
end