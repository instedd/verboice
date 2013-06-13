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
  describe RecordCommand do

    let(:pbx) { double('pbx') }

    before :each do
      pbx.stub :record
    end

    it "should create a recorded audio linking the saved audio file to the call log and contact" do
      contact = Contact.make(:with_address)
      project = contact.project
      call_flow = CallFlow.make project: project
      call_log = CallLog.make call_flow: call_flow

      session = Session.new :pbx => pbx, :call_log => call_log
      session.stub :address => contact.first_address

      cmd = RecordCommand.new 123, 'description'
      cmd.next = :next
      cmd.run(session).should == :next

      RecordedAudio.all.size.size

      RecordedAudio.all.size.should eq(1)
      RecordedAudio.first.call_log.should eq(call_log)
      RecordedAudio.first.contact.should eq(contact)
      RecordedAudio.first.key.should eq('123')
      RecordedAudio.first.description.should eq('description')
    end

    it "should create a Contact if it doesn't exist" do
      call_log = CallLog.make
      session = Session.new :pbx => pbx, :call_log => call_log
      session.stub :address => '1234xxx'

      Contact.all.size.should eq(0)

      cmd = RecordCommand.new 123, 'description'
      cmd.next = :next
      cmd.run(session).should == :next
      Contact.all.size.should eq(1)
      Contact.first.first_address.should eq('1234xxx')
      RecordedAudio.first.contact.should eq(Contact.first)
    end

    it "should create an anonymous contact using the call log id if the contact address is unknown" do
      call_log = CallLog.make id: 456
      session = Session.new :pbx => pbx, :call_log => call_log
      session.stub :address => nil

      Contact.all.size.should eq(0)

      cmd = RecordCommand.new 2, 'foo'
      cmd.next = :next
      cmd.run(session).should == :next
      Contact.all.size.should eq(1)
      Contact.first.first_address.should eq('Anonymous456')
      Contact.first.anonymous?.should eq(true)
      RecordedAudio.first.contact.should eq(Contact.first)
    end

    describe 'pbx' do

      let(:call_log) { CallLog.make }
      let(:session) { Session.new(:pbx => pbx, :call_log => call_log, :address => '1234') }
      let(:filename) { RecordingManager.for(call_log).result_path_for('key') }

      it "should send pbx the record command with params" do
        pbx.should_receive(:record).with(filename, '25#', 5)

        cmd = RecordCommand.new 'key', 'desc', {:stop_keys => '25#', :timeout => 5}
        cmd.run(session)
      end

      it "should send pbx the record command with defaults" do
        pbx.should_receive(:record).with(filename, '01234567890*#', 10)

        cmd = RecordCommand.new 'key', 'desc'
        cmd.run(session)
      end
    end

  end
end
