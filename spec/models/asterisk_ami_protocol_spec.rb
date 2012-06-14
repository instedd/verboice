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

describe Asterisk::AmiProtocol do
  before(:each) do
    @ami = Asterisk::AmiProtocol.new nil
  end

  context "send" do
    it "send" do
      @ami.should_receive(:send_data).with("action: login\n").ordered
      @ami.should_receive(:send_data).with("username: foo\n").ordered
      @ami.should_receive(:send_data).with("secret: bar\n").ordered
      @ami.should_receive(:send_data).with("\n").ordered

      Fiber.new { @ami.login :username => 'foo', :secret => 'bar' }.resume
    end
  end

  context "receive" do
    it "receive response" do
      @ami.should_receive(:resume_fiber_with).with({:response => 'Success', :actionid => 'sarasa', :message => 'something'})

      @ami.receive_line("Asterisk Call Manager/1.1\n")
      @ami.receive_line("Response: Success\n")
      @ami.receive_line("ActionID: sarasa\n")
      @ami.receive_line("Message: something\n")
      @ami.receive_line("\n")
    end

    it "receive response ignores if : not found" do
      @ami.should_receive(:resume_fiber_with).with({:response => 'Success'})

      @ami.receive_line("Asterisk Call Manager/1.1\n")
      @ami.receive_line("Response: Success\n")
      @ami.receive_line(" -- END COMMAND -- \n")
      @ami.receive_line("\n")
    end

    it "receive event" do
      @ami.should_receive(:receive_event).with({:event => 'Hangup', :actionid => 'sarasa', :message => 'something'})

      @ami.receive_line("Asterisk Call Manager/1.1\n")
      @ami.receive_line("Event: Hangup\n")
      @ami.receive_line("ActionID: sarasa\n")
      @ami.receive_line("Message: something\n")
      @ami.receive_line("\n")
    end
  end
end
