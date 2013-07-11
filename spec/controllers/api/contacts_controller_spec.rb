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

describe Api::ContactsController do
  include Devise::TestHelpers

  let!(:account) { Account.make }
  let!(:project) { Project.make account: account }
  let!(:contact) { project.contacts.make }

  before(:each) do
    sign_in account

    ContactAddress.make contact_id: contact.id

    @project_var = project.project_variables.make name: "var1"
    @contact_var = PersistedVariable.make project_variable_id: @project_var.id, contact_id: contact.id, value: "foo"
  end

  it "gets all contacts" do
    get :index, project_id: project.id

    response.should be_ok

    json = JSON.parse response.body
    json.length.should eq(1)

    json = json[0]
    json['id'].should eq(contact.id)
    json['addresses'].should eq(contact.addresses.map(&:address))
    json['vars'].should eq({"var1" => "foo"})
  end

  it "gets contact by address" do
    get :show_by_address, project_id: project.id, address: contact.addresses.first.address

    response.should be_ok

    json = JSON.parse response.body
    json['id'].should eq(contact.id)
    json['addresses'].should eq(contact.addresses.map(&:address))
    json['vars'].should eq({"var1" => "foo"})
  end

  it "updates a contact's var by address" do
    put :update_by_address, project_id: project.id, address: contact.addresses.first.address, vars: {var1: "bar"}

    @contact_var.reload
    @contact_var.value.should eq("bar")

    response.should be_ok

    json = JSON.parse response.body
    json['id'].should eq(contact.id)
    json['addresses'].should eq(contact.addresses.map(&:address))
    json['vars'].should eq({"var1" => "bar"})
  end

  it "updates all contacts vars" do
    put :update_all, project_id: project.id, vars: {var1: "bar"}

    @contact_var.reload
    @contact_var.value.should eq("bar")

    json = JSON.parse response.body
    json.length.should eq(1)

    json = json[0]
    json['id'].should eq(contact.id)
    json['addresses'].should eq(contact.addresses.map(&:address))
    json['vars'].should eq({"var1" => "bar"})
  end
end
