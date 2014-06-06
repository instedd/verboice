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

describe CallFlowsController do
  include Devise::TestHelpers

  before(:each) do
    sign_in account
  end
  let!(:account) { Account.make }
  let!(:project) { Project.make account: account }
  let!(:call_flow) { CallFlow.make project: project }
  let!(:channel) { Channels::Custom.make account: account, name: "Channel1" }

  it 'Should retrieve a csv with the call traces' do
    Timecop.freeze(Time.utc(2012, 1, 1, 0, 0, 0))

    call_flow.user_flow = [{"id"=>1, "name"=>"Initial menu", "type"=>"menu", "root"=>true, "options"=>[{"number"=>1, "next"=>593}, {"number"=>2, "next"=>737}], "end_call_message"=>{"name"=>"Bye", "type"=>"recording", "duration"=>"00:00"}, "invalid_message"=>{"name"=>"Wrong number!", "type"=>"recording", "duration"=>"00:00"}, "explanation_message"=>{"name"=>"Welcome to test call_flow 01", "type"=>"recording", "duration"=>"00:00"}, "options_message"=>{"name"=>"Press 1 for foo, press 2 for bar", "type"=>"recording", "duration"=>"00:00"}}, {"id"=>593, "name"=>"Menu Foo", "type"=>"menu", "root"=>false, "options"=>[{"number"=>1, "next"=>509}, {"number"=>2, "next"=>897}], "end_call_message"=>{}, "invalid_message"=>{}, "explanation_message"=>{"name"=>"You pressed Foo", "type"=>"recording", "duration"=>"00:00"}, "options_message"=>{"name"=>"Press 1 if it's ok, if not, 2", "type"=>"recording", "duration"=>"00:00"}}, {"id"=>737, "name"=>"Menu Bar", "type"=>"menu", "root"=>false, "options"=>[{"number"=>2, "next"=>2}, {"number"=>3, "next"=>3}], "end_call_message"=>{"name"=>"Goodbye", "type"=>"recording", "duration"=>"00:00"}, "invalid_message"=>{}, "explanation_message"=>{"name"=>"You chosed Bar", "type"=>"recording", "duration"=>"00:00"}, "options_message"=>{"name"=>"Press 2 if it's ok, 3 if not", "type"=>"recording", "duration"=>"00:00"}}, {"id"=>509, "name"=>"Menu Ok", "type"=>"menu", "root"=>false, "options"=>[], "end_call_message"=>{"name"=>"Goodbye", "type"=>"recording", "duration"=>"00:00"}, "invalid_message"=>{}, "explanation_message"=>{"name"=>"Thank you for chosing foo", "type"=>"recording", "duration"=>"00:00"}, "options_message"=>{}}, {"id"=>897, "name"=>"Menu not ok", "type"=>"menu", "root"=>false, "options"=>[], "end_call_message"=>{"name"=>"Bye", "type"=>"recording", "duration"=>"00:00"}, "invalid_message"=>{}, "explanation_message"=>{"name"=>"We will call you back later", "type"=>"recording", "duration"=>"00:00"}, "options_message"=>{}}, {"id"=>2, "name"=>"Menu Ok", "type"=>"menu", "root"=>false, "options"=>[], "end_call_message"=>{}, "invalid_message"=>{}, "explanation_message"=>{"name"=>"Ok. Thank you for choosing bar", "type"=>"recording", "duration"=>"00:00"}, "options_message"=>{}}, {"id"=>3, "name"=>"Menu Not ok", "type"=>"menu", "root"=>false, "options"=>[], "end_call_message"=>{}, "invalid_message"=>{}, "explanation_message"=>{"name"=>"Goodbye", "type"=>"recording", "duration"=>"00:00"}, "options_message"=>{}}]
    call_flow.save!

    call_log1 = CallLog.make id: 1, address: 1000, call_flow: call_flow, channel: channel, direction: :incoming, started_at: Time.now, finished_at: Time.now
    call_log2 = CallLog.make id: 2, address: 1000, call_flow: call_flow, channel: channel, direction: :outgoing, started_at: Time.now, finished_at: Time.now
    call_log3 = CallLog.make id: 3, address: 1000, call_flow: call_flow, channel: channel, direction: :incoming, started_at: Time.now, finished_at: Time.now
    call_log4 = CallLog.make id: 4, address: 1000, call_flow: call_flow, channel: channel, direction: :outgoing, started_at: Time.now, finished_at: Time.now
    call_log5 = CallLog.make id: 5, address: 1000, call_flow: call_flow, channel: channel, direction: :incoming, started_at: Time.now, finished_at: Time.now
    call_log6 = CallLog.make id: 6, address: 1000, call_flow: call_flow, channel: channel, direction: :outgoing, started_at: Time.now, finished_at: Time.now

    Hercule::Activity.stub(:search).and_return(hercule_activity_result [
      { 'call_log_id' => call_log3.id, 'step_id' => 1, 'step_result' => 'timeout' },
      { 'call_log_id' => call_log1.id, 'step_id' => 1, 'step_result' => 'pressed', 'step_data' => '2' },
      { 'call_log_id' => call_log2.id, 'step_id' => 1, 'step_result' => 'pressed', 'step_data' => '1' },
      { 'call_log_id' => call_log2.id, 'step_id' => 593, 'step_result' => 'pressed', 'step_data' => '1' },
      { 'call_log_id' => call_log4.id, 'step_id' => 1, 'step_result' => 'pressed', 'step_data' => '1' },
      { 'call_log_id' => call_log4.id, 'step_id' => 593, 'step_result' => 'pressed', 'step_data' => '2' },
      { 'call_log_id' => call_log5.id, 'step_id' => 1, 'step_result' => 'pressed', 'step_data' => '2' },
      { 'call_log_id' => call_log5.id, 'step_id' => 737, 'step_result' => 'timeout' },
      { 'call_log_id' => call_log6.id, 'step_id' => 1, 'step_result' => 'pressed', 'step_data' => '2' },
      { 'call_log_id' => call_log6.id, 'step_id' => 737, 'step_result' => 'pressed', 'step_data' => '2' },
      { 'call_log_id' => call_log5.id, 'step_id' => 43212345678, 'step_result' => 'pressed', 'step_data' => '2' },
    ])

    response = get :download_results, :format => :csv, id: call_flow.id, project_id: call_flow.project.id
    response.body.should eq File.read(File.join(Rails.root, 'spec/fixtures/trace.csv'))

    Timecop.return
  end


  describe "GET index" do
    it "assigns all call_flows as @call_flows" do
      get :index, {:project_id => project.to_param}
      assigns(:call_flows).should eq([call_flow])
    end
  end

  describe "POST create" do
    describe "with valid params" do
      it "creates a new CallFlow" do
        expect {
          post :create, {:call_flow => CallFlow.plan, :project_id => project.to_param}
        }.to change(CallFlow, :count).by(1)
      end

      it "assigns a newly created call_flow as @call_flow" do
        post :create, {:call_flow => CallFlow.plan, :project_id => project.to_param}
        assigns(:call_flow).should be_a(CallFlow)
        assigns(:call_flow).should be_persisted
      end

      it "redirects to the created call_flow" do
        post :create, {:call_flow => CallFlow.plan, :project_id => project.to_param}
        response.should render_template("index")
      end
    end

    describe "with invalid params" do
      it "assigns a newly created but unsaved call_flow as @call_flow" do
        CallFlow.any_instance.stub(:save).and_return(false)
        post :create, {:call_flow => {}, :project_id => project.to_param}
        assigns(:call_flow).should be_a_new(CallFlow)
      end

      it "re-renders the 'new' template" do
        CallFlow.any_instance.stub(:save).and_return(false)
        post :create, {:call_flow => {}, :project_id => project.to_param}
        response.should render_template("index")
      end
    end

    it "creates a new CallFlow in shared project" do
      other_project = Project.make
      Permission.create!(account_id: account.id, type: "Project", model_id: other_project.id, role: :admin)
      expect {
        post :create, {:call_flow => CallFlow.plan, :project_id => other_project.to_param}
      }.to change(CallFlow, :count).by(1)
    end

    it "cannot create a new CallFlow in shared project without admin role" do
      other_project = Project.make
      Permission.create!(account_id: account.id, type: "Project", model_id: other_project.id, role: :user)
      post :create, {:call_flow => CallFlow.plan, :project_id => other_project.to_param}
      response.status.should eq(401)
    end
  end

  describe "PUT update" do
    describe "with valid params" do
      it "updates the requested call_flow" do
        CallFlow.any_instance.should_receive(:update_attributes).with({'these' => 'params'})
        put :update, {:id => call_flow.to_param, :call_flow => {'these' => 'params'}, :project_id => project.to_param}
      end

      it "assigns the requested call_flow as @call_flow" do
        put :update, {:id => call_flow.to_param, :call_flow => CallFlow.plan, :project_id => project.to_param}
        assigns(:call_flow).should eq(call_flow)
      end

      it "redirects to the call_flow" do
        put :update, {:id => call_flow.to_param, :call_flow => CallFlow.plan, :project_id => project.to_param}
        response.should render_template("index")
      end
    end

    describe "with invalid params" do
      it "assigns the call_flow as @call_flow" do
        CallFlow.any_instance.stub(:save).and_return(false)
        put :update, {:id => call_flow.to_param, :call_flow => {}, :project_id => project.to_param}
        assigns(:call_flow).should eq(call_flow)
      end

      it "re-renders the 'edit' template" do
        CallFlow.any_instance.stub(:save).and_return(false)
        put :update, {:id => call_flow.to_param, :call_flow => {}, :project_id => project.to_param}
        response.should render_template("index")
      end
    end
  end

  describe "DELETE destroy" do
    it "destroys the requested call_flow" do
      expect {
        delete :destroy, {:id => call_flow.to_param, :project_id => project.to_param}
      }.to change(CallFlow, :count).by(-1)
    end

    it "redirects to the call_flows list" do
      delete :destroy, {:id => call_flow.to_param, :project_id => project.to_param}
      response.should redirect_to(project_call_flows_path(project))
    end
  end

  private

  def hercule_activity_result(activity_fields)
    Hercule::Activity::Result.new({
      'hits' => {
        'total' => activity_fields.length,
        'hits' => activity_fields.map do |fields|
          { '_source' => { '@fields' => fields } }
        end
      }
    })
  end

end
