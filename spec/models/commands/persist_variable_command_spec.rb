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
  describe PersistVariableCommand do
    it "should create a persisted variable storing a value with a given name" do
      contact  = Contact.make
      project  = contact.project
      call_flow = CallFlow.make project: project
      call_log = CallLog.make call_flow: call_flow
      session  = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => contact.first_address

      cmd = PersistVariableCommand.new 'foo', 2
      cmd.next = :next
      cmd.run(session).should eq(:next)

      PersistedVariable.all.size.should eq(1)
      PersistedVariable.first.value.should eq('2')
      ProjectVariable.all.size.should eq(1)
      ProjectVariable.first.name.should eq('foo')
      PersistedVariable.first.project_variable.should eq(ProjectVariable.first)
      PersistedVariable.first.contact.should eq(contact)
      session["var_foo"].should eq(2)
    end

    it "should create a Contact if it doesn't exist" do
      call_log = CallLog.make
      session  = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => '1234xxx'

      cmd = PersistVariableCommand.new 'foo', 2
      cmd.next = :next
      cmd.run(session).should eq(:next)
      Contact.all.size.should eq(1)
      Contact.first.first_address.should eq('1234xxx')
      PersistedVariable.first.contact.should eq(Contact.first)
    end

    it "should replace the value of an existing variable" do
      contact  = Contact.make
      project  = contact.project
      call_flow = CallFlow.make project: project
      call_log = CallLog.make call_flow: call_flow
      session  = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => contact.first_address

      cmd = PersistVariableCommand.new 'foo', 2
      cmd.next = :next
      cmd.run(session).should eq(:next)

      PersistedVariable.all.size.should eq(1)
      PersistedVariable.first.value.should eq('2')
      ProjectVariable.all.size.should eq(1)
      ProjectVariable.first.name.should eq('foo')
      PersistedVariable.first.project_variable.should eq(ProjectVariable.first)
      PersistedVariable.first.contact.should eq(contact)

      cmd = PersistVariableCommand.new 'foo', 1
      cmd.next = :next
      cmd.run(session).should eq(:next)

      PersistedVariable.all.size.should eq(1)
      PersistedVariable.first.value.should eq('1')
      ProjectVariable.all.size.should eq(1)
      ProjectVariable.first.name.should eq('foo')
      PersistedVariable.first.project_variable.should eq(ProjectVariable.first)
      PersistedVariable.first.contact.should eq(contact)
    end

    it "should create an anonymous contact using the call log id if the contact address is unknown" do
      call_log = CallLog.make id: 123
      session  = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => nil

      cmd = PersistVariableCommand.new 'foo', 2
      cmd.next = :next
      cmd.run(session).should eq(:next)
      Contact.all.size.should eq(1)
      Contact.first.first_address.should eq('Anonymous123')
      Contact.first.anonymous?.should eq(true)
      PersistedVariable.first.contact.should eq(Contact.first)
    end


    it "should persist implicit variables" do
      contact  = Contact.make
      project  = contact.project
      call_flow = CallFlow.make project: project
      call_log = CallLog.make call_flow: call_flow
      session  = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => contact.first_address

      cmd = PersistVariableCommand.new ImplicitVariables::Language.key, "'kh'"
      cmd.run(session)

      persisted_variables = contact.reload.persisted_variables
      persisted_variables.size.should eq(1)
      persisted_variables.first.implicit_key.should eq(ImplicitVariables::Language.key)
      persisted_variables.first.value.should eq('kh')
      persisted_variables.first.project_variable.should be_nil
    end

    it "should update value of persisted implicit variables" do
      contact  = Contact.make
      project  = contact.project
      call_flow = CallFlow.make project: project
      call_log = CallLog.make call_flow: call_flow
      session  = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => contact.first_address
      # PersistedVariable.

      cmd = PersistVariableCommand.new ImplicitVariables::Language.key, "'kh'"
      cmd.run(session)

      persisted_variables = contact.reload.persisted_variables
      persisted_variables.size.should eq(1)
      persisted_variables.first.implicit_key.should eq(ImplicitVariables::Language.key)
      persisted_variables.first.value.should eq('kh')
      persisted_variables.first.project_variable.should be_nil
    end
  end
end
