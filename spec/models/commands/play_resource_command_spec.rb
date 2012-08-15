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

    it "should work with TextLocalizedResource" do
      text_localized_resource = TextLocalizedResource.make text: 'some text'
      call_flow = CallFlow.make project: text_localized_resource.project
      session = Session.new pbx: double('pbx'), call_log: CallLog.make, call_flow: call_flow

      session.pbx.should_receive(:say).with('some text', anything).and_return(:foo)

      cmd = PlayResourceCommand.new text_localized_resource.resource.guid
      cmd.next = :next
      cmd.run(session).should == :next
    end


    it "should work with UploadLocalizedResource" do
      localized_resource = UploadLocalizedResource.make
      call_flow = CallFlow.make project: localized_resource.project
      session = Session.new pbx: double('pbx'), call_log: CallLog.make, call_flow: call_flow

      play_command = double('play_command')
      PlayAudioCommand.stub(:new).with(anything).and_return(play_command)
      play_command.stub(:should_setup_file?).with(anything).and_return(false)
      play_command.stub(:run).and_return(:foo)

      cmd = PlayResourceCommand.new localized_resource.resource.guid
      cmd.next = :next
      cmd.run(session).should == :next
    end

    it "should work with RecordLocalizedResource" do
      localized_resource = RecordLocalizedResource.make
      call_flow = CallFlow.make project: localized_resource.project
      session = Session.new pbx: double('pbx'), call_log: CallLog.make, call_flow: call_flow

      play_command = double('play_command')
      PlayAudioCommand.stub(:new).with(anything).and_return(play_command)
      play_command.stub(:should_setup_file?).with(anything).and_return(false)
      play_command.stub(:run).and_return(:foo)

      cmd = PlayResourceCommand.new localized_resource.resource.guid
      cmd.next = :next
      cmd.run(session).should == :next
    end

    it "should work with UrlLocalizedResource" do
      localized_resource = UrlLocalizedResource.make
      call_flow = CallFlow.make project: localized_resource.project
      session = Session.new pbx: double('pbx'), call_log: CallLog.make, call_flow: call_flow

      play_command = double('play_command')
      PlayUrlCommand.stub(:new).with(anything).and_return(play_command)
      play_command.stub(:should_setup_file?).with(anything).and_return(false)
      play_command.stub(:run).and_return(:foo)

      cmd = PlayResourceCommand.new localized_resource.resource.guid
      cmd.next = :next
      cmd.run(session).should == :next
    end
  end
end
