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
  describe PlayResourceCommand do
    let(:session) { Session.new pbx: double('pbx'), call_log: CallLog.make}
    let(:localized_resource) { LocalizedResource.make text: 'some text' }

    it "returns next command" do
      session.pbx.should_receive(:say).with('some text', anything).and_return(:foo)

      cmd = PlayResourceCommand.new localized_resource.resource.id
      cmd.next = :next
      cmd.run(session).should == :next
    end

  end
end
