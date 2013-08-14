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
  describe NuntiumCommand do
    let(:project) { Project.make }
    let(:call_flow) { CallFlow.make project: project }
    let(:call_log) { CallLog.make project: project, call_flow: call_flow }
    let(:resource) { Resource.make project: project }
    let(:nuntium) { double('nuntium') }
    let(:session) { Session.new address: '123', call_log: call_log, call_flow: call_flow }

    it "should work with TextLocalizedResource" do
      text_localized_resource = TextLocalizedResource.make text: 'some text', resource: resource
      nuntium.should_receive(:send_ao).with(:from => 'sms://verboice', :to => 'sms://123', :body => 'some text', :account_id => project.account_id)

      cmd = NuntiumCommand.new resource.guid, :caller
      cmd.should_receive(:nuntium).and_return(nuntium)
      cmd.next = :next
      cmd.run(session).should == :next
    end

    it "should not work with UrlLocalizedResource" do
      url_localized_resource = UrlLocalizedResource.make resource: resource

      cmd = NuntiumCommand.new resource.guid, :caller
      cmd.should_receive(:nuntium).never
      cmd.next = :next
      cmd.run(session).should == :next
    end

    it "should not work with UploadLocalizedResource" do
      url_localized_resource = UploadLocalizedResource.make resource: resource

      cmd = NuntiumCommand.new resource.guid, :caller
      cmd.should_receive(:nuntium).never
      cmd.next = :next
      cmd.run(session).should == :next
    end

    it "should not work with RecordLocalizedResource" do
      url_localized_resource = RecordLocalizedResource.make resource: resource

      cmd = NuntiumCommand.new resource.guid, :caller
      cmd.should_receive(:nuntium).never
      cmd.next = :next
      cmd.run(session).should == :next
    end

    it "should expand vars in the text message" do
      text_localized_resource = TextLocalizedResource.make text: 'hello {name}', resource: resource

      session['var_name'] = 'world'
      nuntium.should_receive(:send_ao).with(:from => 'sms://verboice', :to => 'sms://123', :body => 'hello world', :account_id => project.account_id)

      cmd = NuntiumCommand.new resource.guid, :caller
      cmd.should_receive(:nuntium).and_return(nuntium)
      cmd.next = :next
      cmd.run(session).should == :next
    end

    it "should send to a value recipient" do
      text_localized_resource = TextLocalizedResource.make text: 'some text', resource: resource

      nuntium.should_receive(:send_ao).with(:from => 'sms://verboice', :to => 'sms://555', :body => 'some text', :account_id => project.account_id)

      cmd = NuntiumCommand.new resource.guid, :expr, "'555'"
      cmd.should_receive(:nuntium).and_return(nuntium)
      cmd.next = :next
      cmd.run(session).should == :next
    end

    it "should send to a variable recipient" do
      text_localized_resource = TextLocalizedResource.make text: 'some text', resource: resource

      nuntium.should_receive(:send_ao).with(:from => 'sms://verboice', :to => 'sms://456', :body => 'some text', :account_id => project.account_id)
      session['var_rcpt'] = '456'

      cmd = NuntiumCommand.new resource.guid, :expr, 'var_rcpt'
      cmd.should_receive(:nuntium).and_return(nuntium)
      cmd.next = :next
      cmd.run(session).should == :next
    end

    it "should not add the protocol prefix if present" do
      text_localized_resource = TextLocalizedResource.make text: 'some text', resource: resource

      nuntium.should_receive(:send_ao).with(:from => 'sms://verboice', :to => 'xmpp://foo@bar', :body => 'some text', :account_id => project.account_id)
      session['var_rcpt'] = 'xmpp://foo@bar'

      cmd = NuntiumCommand.new resource.guid, :expr, 'var_rcpt'
      cmd.should_receive(:nuntium).and_return(nuntium)
      cmd.next = :next
      cmd.run(session).should == :next
    end

    ['var_nonexistant', 'null', 'undefined', nil].each do |expr|
      it "should not work if the expression is #{expr}" do
        text_localized_resource = TextLocalizedResource.make text: 'some text', resource: resource

        cmd = NuntiumCommand.new resource.guid, :expr, expr
        cmd.should_receive(:nuntium).never
        cmd.next = :next
        cmd.run(session).should == :next
      end
    end

    it "should use the implicit variable sms_number if present" do
      contact = project.contacts.make
      contact.addresses.create! address: '123'
      contact.persisted_variables.create! :implicit_key => 'sms_number', :value => '456'
      session.load_variables

      text_localized_resource = TextLocalizedResource.make text: 'some text', resource: resource
      nuntium.should_receive(:send_ao).with(:from => 'sms://verboice', :to => 'sms://456', :body => 'some text', :account_id => project.account_id)

      cmd = NuntiumCommand.new resource.guid, :caller
      cmd.should_receive(:nuntium).and_return(nuntium)
      cmd.next = :next
      cmd.run(session).should == :next
    end
  end
end
