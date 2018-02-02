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

describe ContactsController do
  before(:each) do
    @account = Account.make
    @project = @account.projects.make
    @other_project = Project.make
    sign_in @account
  end

  let!(:contact) { Contact.make :project => @project }
  let!(:other_contact) { Contact.make :project => @other_project }

  describe "GET index" do
    it "assigns all project contacts as @contacts" do
      get :index, {:project_id => @project.id}
      expect(assigns(:contacts)).to eq([contact])
    end
  end

  describe "GET index CSV" do
    let!(:project_csv) { @account.projects.make }

    let!(:project_variable) { project_csv.project_variables.make name: 'ProjectVar' }

    let!(:call_flow) { project_csv.call_flows.make name: "Callflow 1" }

    let!(:channel) { Channels::Custom.make call_flow: call_flow, name: "Channel 1" }

    let!(:contact1) { Contact.make :project => project_csv, addresses_attributes: [{address: '1234'}, {address: '5678'}] }

    let!(:contact2) { Contact.make :project => project_csv, addresses_attributes: [{address: '0123'}] }

    let!(:contact1_lang) { contact1.persisted_variables.make implicit_key: 'language', value: 'es' }

    let!(:contact2_var) { contact2.persisted_variables.make project_variable: project_variable, value: 'vartest' }

    def called(address, date, options)
      Timecop.freeze date
      call_flow.call_logs.make account_id: project_csv.account_id, project_id: project_csv.id, address: address, state: options[:state] ? options[:state] : :failed, channel: channel
      Timecop.return
    end

    it "renders as CSV" do
      called '1234', Time.utc(2012, 1, 1, 0, 0, 0), state: :completed
      called '5678', Time.utc(2013, 1, 1, 0, 0, 0), state: :failed
      called '0123', Time.utc(2013, 1, 1, 0, 0, 0), state: :failed
      called '0123', Time.utc(2014, 1, 1, 0, 0, 0), state: :failed

      response = get :index, format: :csv, project_id: project_csv.id
      expect(response.body).to eq File.read(File.join(Rails.root, 'spec/fixtures/phone_book.csv'))
    end
  end

  describe "GET new" do
    it "assigns a new contact as @contact" do
      get :new, {:project_id => @project.id}
      expect(assigns(:contact)).to be_a_new(Contact)
    end
  end

  describe "GET edit" do
    it "assigns the requested contact as @contact" do
      get :edit, {:project_id => @project.id, :id => contact.to_param}
      expect(assigns(:contact)).to eq(contact)
    end

    it "fails if the requested contact is not in current project" do
      expect {
        get :edit, {:project_id => @project.id, :id => other_contact.to_param}
      }.to raise_error(ActiveRecord::RecordNotFound)
      expect(assigns(:contact)).to be_nil
    end
  end

  describe "POST create" do
    describe "with valid params" do
      it "creates a new Contact" do
        expect {
          post :create, {:project_id => @project.id, :contact => {:addresses_attributes => [{:address => '123'}]}}
        }.to change(Contact, :count).by(1)
      end

      it "assigns a newly created contact as @contact" do
        post :create, {:project_id => @project.id, :contact => {:addresses_attributes => [{:address => '123'}]}}
        expect(assigns(:contact)).to be_a(Contact)
        expect(assigns(:contact)).to be_persisted
      end

      it "redirects to index" do
        post :create, {:project_id => @project.id, :contact => {:addresses_attributes => [{:address => '123'}]}}
        expect(response).to redirect_to(project_contacts_url(@project))
      end

      it "assigns the current project to the contact" do
        post :create, {:project_id => @project.id, :contact => {:addresses_attributes => [{:address => '123'}]}}
        expect(assigns(:contact).project).to eq(@project)
      end
    end

    describe "with invalid params" do
      it "assigns a newly created but unsaved contact as @contact" do
        allow_any_instance_of(Contact).to receive(:save).and_return(false)
        post :create, {:project_id => @project.id, :contact => {}}
        expect(assigns(:contact)).to be_a_new(Contact)
      end

      it "re-renders the 'new' template" do
        allow_any_instance_of(Contact).to receive(:save).and_return(false)
        post :create, {:project_id => @project.id, :contact => {}}
        expect(response).to render_template("new")
      end
    end
  end

  describe "PUT update" do
    describe "with valid params" do
      it "updates the requested contact" do
        expect_any_instance_of(Contact).to receive(:update_attributes).with({'these' => 'params'})
        put :update, {:project_id => @project.id, :id => contact.to_param, :contact => {'these' => 'params'}}
      end

      it "assigns the requested contact as @contact" do
        put :update, {:project_id => @project.id, :id => contact.to_param, :contact => Contact.plan}
        expect(assigns(:contact)).to eq(contact)
      end

      it "redirects to index" do
        put :update, {:project_id => @project.id, :id => contact.to_param, :contact => Contact.plan}
        expect(response).to redirect_to(project_contacts_url(@project))
      end
    end

    describe "with invalid params" do
      it "assigns the contact as @contact" do
        allow_any_instance_of(Contact).to receive(:save).and_return(false)
        put :update, {:project_id => @project.id, :id => contact.to_param, :contact => {}}
        expect(assigns(:contact)).to eq(contact)
      end

      it "re-renders the 'edit' template" do
        allow_any_instance_of(Contact).to receive(:save).and_return(false)
        put :update, {:project_id => @project.id, :id => contact.to_param, :contact => {}}
        expect(response).to render_template("edit")
      end
    end

    it "fails if the requested contact is not in current project" do
      expect {
        put :update, {:project_id => @project.id, :id => other_contact.to_param}
      }.to raise_error(ActiveRecord::RecordNotFound)
      expect(assigns(:contact)).to be_nil
    end
  end

  describe "DELETE destroy" do
    it "destroys the requested contact" do
      expect {
        delete :destroy, {:project_id => @project.id, :id => contact.to_param}
      }.to change(Contact, :count).by(-1)
    end

    it "redirects to the contacts list" do
      delete :destroy, {:project_id => @project.id, :id => contact.to_param}
      expect(response).to redirect_to(project_contacts_url(@project))
    end

    it "fails if the requested contact is not in current project" do
      expect {
        delete :destroy, {:project_id => @project.id, :id => other_contact.to_param}
      }.to raise_error(ActiveRecord::RecordNotFound)
      expect(assigns(:contact)).to be_nil
      expect(Contact.find(other_contact.id)).to eq(other_contact)
    end
  end

  describe "with shared project" do
    before(:each) do
      Permission.create!(account_id: @account.id, type: "Project", model_id: @other_project.id, role: :admin)
    end

    it "can view contacts" do
      get :index, {:project_id => @other_project.id}
      expect(assigns(:contacts)).to eq([other_contact])
    end

    it "edit contact" do
      get :edit, {:project_id => @other_project.id, :id => other_contact.to_param}
      expect(assigns(:contact)).to eq(other_contact)
    end

    it "destroy the requested contact from shared project" do
      expect {
        delete :destroy, {:project_id => @other_project.id, :id => other_contact.to_param}
      }.to change(Contact, :count).by(-1)
    end
  end

end
