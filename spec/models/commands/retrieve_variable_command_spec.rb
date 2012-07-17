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
  describe RetrieveVariableCommand do
    it "should retrieve a persisted numeric variable" do
      project             = Project.make
      contact             = Contact.make project: project
      project_variable    = ProjectVariable.make name: 'foo', project: project
      persisted_variable  = PersistedVariable.make project_variable: project_variable, contact: contact
      call_flow           = CallFlow.make project: project
      call_log            = CallLog.make call_flow: call_flow
      session             = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => contact.address

      cmd = RetrieveVariableCommand.new 'foo'
      cmd.next = :next

      cmd.run(session).should eq(:next)
      session.eval('var_foo').should eq(persisted_variable.value.to_i)
    end

    it "should retrieve a persisted string variable" do
      project           = Project.make
      contact           = Contact.make project: project
      project_variable  = ProjectVariable.make name: 'varname', project: project
      PersistedVariable.make project_variable: project_variable, value: 'thevalue', contact: contact
      call_flow         = CallFlow.make project: project
      call_log          = CallLog.make call_flow: call_flow
      session           = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => contact.address

      cmd = RetrieveVariableCommand.new 'varname'
      cmd.next = :next

      cmd.run(session).should eq(:next)
      session.eval('var_varname').should eq('thevalue')
    end


    it "should set to nil if the contact doesn't exist" do
      call_log = CallLog.make
      session  = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => '1234xxxx'

      cmd = RetrieveVariableCommand.new 'foo'
      cmd.next = :next

      cmd.run(session).should eq(:next)
      session.eval('var_foo').should eq(nil)
    end

    it "should set to nil if the variable doesn't exist" do
      project   = Project.make
      contact   = Contact.make project: project
      call_flow = CallFlow.make project: project
      call_log  = CallLog.make call_flow: call_flow
      session   = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => contact.address

      cmd = RetrieveVariableCommand.new 'foo'
      cmd.next = :next

      cmd.run(session).should eq(:next)
      session.eval('var_foo').should eq(nil)
    end

    it "should set to the value from an anonymous contact based on the call id if the caller address is unknown" do
      project            = Project.make
      contact            = Contact.make address: 'Anonymous44', project: project
      project_variable   = ProjectVariable.make name: 'foo', project: project
      persisted_variable = PersistedVariable.make project_variable: project_variable, contact: contact
      call_flow          = CallFlow.make project: project
      call_log           = CallLog.make call_flow: call_flow, id: 44
      session            = Session.new :pbx => mock('pbx'), :call_log => call_log
      session.stub :address => nil

      cmd = RetrieveVariableCommand.new 'foo'
      cmd.next = :next

      cmd.run(session).should eq(:next)
      session.eval('var_foo').should eq(persisted_variable.value.to_i)
    end

     it "should set the value to nil if the caller address is unknown and the anonymous contact doesn't exist" do
       call_log = CallLog.make
       session  = Session.new :pbx => mock('pbx'), :call_log => call_log
       session.stub :address => nil

       cmd = RetrieveVariableCommand.new 'foo'
       cmd.next = :next

       cmd.run(session).should eq(:next)
       session.eval('var_foo').should eq(nil)
     end

     it "should set to nil if the address is unknown and the variable doesn't exist" do
       contact  = Contact.make address: 'Anonymous2'
       project  = contact.project
       call_log = CallLog.make project: project, id: 2
       session  = Session.new :pbx => mock('pbx'), :call_log => call_log
       session.stub :address => nil

       cmd = RetrieveVariableCommand.new 'foo'
       cmd.next = :next

       cmd.run(session).should eq(:next)
       session.eval('var_foo').should eq(nil)
     end

     it "should retrieve default value of implicit variable" do
       project             = Project.make :default_language => 'en'
       contact             = Contact.make project: project
       call_flow           = CallFlow.make project: project
       call_log            = CallLog.make call_flow: call_flow
       session             = Session.new :pbx => mock('pbx'), :call_log => call_log
       session.stub :address => contact.address

       cmd = RetrieveVariableCommand.new ImplicitVariables::Language.key

       cmd.run(session)
       session.eval("var_#{ImplicitVariables::Language.key}").should eq('en')
     end

     it "should retrieve persisted value of implicit variable" do
        project             = Project.make :default_language => 'en'
        contact             = Contact.make project: project
        persisted_variable  = PersistedVariable.make implicit_key: ImplicitVariables::Language.key, contact: contact, value: 'kh'
        call_flow           = CallFlow.make project: project
        call_log            = CallLog.make call_flow: call_flow
        session             = Session.new :pbx => mock('pbx'), :call_log => call_log
        session.stub :address => contact.address

        cmd = RetrieveVariableCommand.new ImplicitVariables::Language.key

        cmd.run(session)
        session.eval("var_#{ImplicitVariables::Language.key}").should eq('kh')
      end

  end
end
