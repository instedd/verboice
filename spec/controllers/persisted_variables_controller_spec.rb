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

describe PersistedVariablesController do
  include Devise::TestHelpers

  before(:each) do
    sign_in account
  end
  let!(:account) { Account.make }
  let!(:contact) { Contact.make account: account }
  let!(:other_contact) { Contact.make }
  let!(:persisted_variable) { PersistedVariable.make contact: contact }
  let!(:other_persisted_variable) { PersistedVariable.make contact: other_contact }

  describe "GET index" do
    it "assigns all persisted_variables as @persisted_variables" do
      get :index, {:contact_id => contact.to_param}
      assigns(:persisted_variables).should eq([persisted_variable])
    end
  end

  describe "GET show" do
    it "assigns the requested persisted_variable as @persisted_variable" do
      get :show, {:id => persisted_variable.to_param, :contact_id => contact.to_param}
      assigns(:persisted_variable).should eq(persisted_variable)
    end

    it "fails if the requested persisted_variable is not in current account contacts" do
      expect {
        get :show, {:id => other_persisted_variable.to_param, :contact_id => other_contact.to_param}
      }.should raise_error
      assigns(:persisted_variable).should be_nil
    end
  end

  describe "GET new" do
    it "assigns a new persisted_variable as @persisted_variable" do
      get :new, {:contact_id => contact.to_param}
      assigns(:persisted_variable).should be_a_new(PersistedVariable)
    end

    it "assigns the contact to the new persisted_variable" do
      get :new, {:contact_id => contact.to_param}
      assigns(:persisted_variable).contact.should eq(contact)
    end
  end

  describe "GET edit" do
    it "assigns the requested persisted_variable as @persisted_variable" do
      get :edit, {:contact_id => contact.to_param, :id => persisted_variable.to_param}
      assigns(:persisted_variable).should eq(persisted_variable)
    end

    it "fails if the requested persisted_variable is not in current account contacts" do
      expect {
        get :edit, {:id => other_persisted_variable.to_param, :contact_id => other_contact.to_param}
      }.should raise_error
      assigns(:persisted_variable).should be_nil
    end
  end

  describe "POST create" do
    describe "with valid params" do
      it "creates a new PersistedVariable" do
        expect {
          post :create, {:persisted_variable => PersistedVariable.plan, :contact_id => contact.to_param}
        }.to change(PersistedVariable, :count).by(1)
      end

      it "assigns a newly created persisted_variable as @persisted_variable" do
        post :create, {:persisted_variable => PersistedVariable.plan, :contact_id => contact.to_param}
        assigns(:persisted_variable).should be_a(PersistedVariable)
        assigns(:persisted_variable).should be_persisted
      end

      it "redirects to the created persisted_variable" do
        post :create, {:persisted_variable => PersistedVariable.plan, :contact_id => contact.to_param}
        response.should redirect_to([contact, contact.persisted_variables.last])
      end
    end

    describe "with invalid params" do
      it "assigns a newly created but unsaved persisted_variable as @persisted_variable" do
        PersistedVariable.any_instance.stub(:save).and_return(false)
        post :create, {:persisted_variable => {}, :contact_id => contact.to_param}
        assigns(:persisted_variable).should be_a_new(PersistedVariable)
      end

      it "re-renders the 'new' template" do
        PersistedVariable.any_instance.stub(:save).and_return(false)
        post :create, {:persisted_variable => {}, :contact_id => contact.to_param}
        response.should render_template("new")
      end
    end

    it "fails if the requested contact is not in current account contacts" do
      expect {
        post :create, {:persisted_variable => PersistedVariable.plan, :contact_id => other_contact.to_param}
      }.should raise_error
      assigns(:persisted_variable).should_not be_persisted
    end
  end

  describe "PUT update" do
    describe "with valid params" do
      it "updates the requested persisted_variable" do
        PersistedVariable.any_instance.should_receive(:update_attributes).with({'these' => 'params'})
        put :update, {:id => persisted_variable.to_param, :persisted_variable => {'these' => 'params'}, :contact_id => contact.to_param}
      end

      it "assigns the requested persisted_variable as @persisted_variable" do
        put :update, {:id => persisted_variable.to_param, :persisted_variable => PersistedVariable.plan, :contact_id => contact.to_param}
        assigns(:persisted_variable).should eq(persisted_variable)
      end

      it "redirects to the persisted_variable" do
        put :update, {:id => persisted_variable.to_param, :persisted_variable => PersistedVariable.plan, :contact_id => contact.to_param}
        response.should redirect_to([contact, contact.persisted_variables.last])
      end
    end

    describe "with invalid params" do
      it "assigns the persisted_variable as @persisted_variable" do
        PersistedVariable.any_instance.stub(:save).and_return(false)
        put :update, {:id => persisted_variable.to_param, :persisted_variable => {}, :contact_id => contact.to_param}
        assigns(:persisted_variable).should eq(persisted_variable)
      end

      it "re-renders the 'edit' template" do
        PersistedVariable.any_instance.stub(:save).and_return(false)
        put :update, {:id => persisted_variable.to_param, :persisted_variable => {}, :contact_id => contact.to_param}
        response.should render_template("edit")
      end
    end

    it "fails if the requested persisted_variable is not in current account contacts" do
      expect {
        put :update, {:id => other_persisted_variable.to_param, :contact_id => other_contact.to_param}
      }.should raise_error
      assigns(:persisted_variable).should be_nil
    end
  end

  describe "DELETE destroy" do
    it "destroys the requested persisted_variable" do
      expect {
        delete :destroy, {:id => persisted_variable.to_param, :contact_id => contact.to_param}
      }.to change(PersistedVariable, :count).by(-1)
    end

    it "redirects to the persisted_variables list" do
      delete :destroy, {:id => persisted_variable.to_param, :contact_id => contact.to_param}
      response.should redirect_to(contact_persisted_variables_path(contact))
    end

    it "fails if the requested persisted_variable is not in current account contacts" do
      expect {
        delete :destroy, {:id => other_persisted_variable.to_param, :contact_id => other_contact.to_param}
      }.should raise_error
      assigns(:persisted_variable).should be_nil
      PersistedVariable.find(other_persisted_variable.id).should eq(other_persisted_variable)
    end
  end

end
