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

describe ProjectsController do
  include Devise::TestHelpers

  let!(:account) { Account.make }
  let!(:project) { Project.make :account => account }

  before(:each) do
    sign_in account
  end

  context "CRUD" do
    it "should edit a project" do
      get :edit, :id => project.id
      response.should be_successful
      assigns(:project).should eq(project)
    end

    it "should update a project" do
      put :update, :id => project.id, :project => {:name => 'My New Project Name', :time_zone => 'GMT-3'}
      response.should be_redirect
      project.reload.name.should eq('My New Project Name')
      project.reload.time_zone.should eq('GMT-3')
    end

    it "delete a project" do
      expect {
        delete :destroy, id: project.to_param
      }.to change(Project, :count).by(-1)
    end

    it "should not be able to delete a shared project" do
      other_project = Project.make
      Permission.create!(account_id: account.id, type: "Project", model_id: other_project.id, role: :admin)
      expect {
        delete :destroy, id: other_project.to_param
      }.to raise_error(ActiveRecord::RecordNotFound)
    end
  end

  context "Call enqueue:" do
    let!(:call_flow) { CallFlow.make :project => project }
    let!(:channel) { Channel.all_leaf_subclasses.sample.make :call_flow => call_flow, :account => account }
    let!(:schedule) { project.schedules.make :weekdays => "1", :time_to => Time.utc(2012, 1, 1, 23, 59, 59)}

    before(:each) do
      Timecop.freeze(Time.local(2012, 1, 1, 0, 0, 0))
      BrokerClient.stub(:notify_call_queued)
    end

    after(:each) do
      Timecop.return
    end

    it 'should enqueue a call' do
      expect {
        post :enqueue_call, :id => project.id, :addresses => "1", :channel_id => channel.id, :schedule_id => schedule.id, :call_flow_id => call_flow.id
      }.to change(QueuedCall, :count).by(1)
      response.should be_redirect
    end

    it 'should ignore the not before date if not before check is not set' do
      not_before = DateTime.new(2012, 1, 1, 16, 0, 0)

      BrokerClient.should_receive(:notify_call_queued).with(channel.id, anything)

      expect {
        post :enqueue_call, :id => project.id, :addresses => "1", :channel_id => channel.id, :schedule_id => schedule.id, :call_flow_id => call_flow.id, :not_before_date => not_before
      }.to change(QueuedCall, :count).by(1)

      enqueued_call = QueuedCall.last
      enqueued_call.schedule_id.should eq(schedule.id)
      enqueued_call.project_id.should eq(project.id)
      enqueued_call.not_before.should_not eq(not_before + 1)

      response.should be_redirect
    end

    it 'should enqueue a call not before specific date' do
      not_before = DateTime.new(2012, 1, 1, 16, 0, 0)

      BrokerClient.should_receive(:notify_call_queued).with(channel.id, not_before + 1.day)

      expect {
        post :enqueue_call, :id => project.id, :addresses => "1", :channel_id => channel.id, :schedule_id => schedule.id, :call_flow_id => call_flow.id, :not_before_date => not_before, :not_before => true
      }.to change(QueuedCall, :count).by(1)

      enqueued_call = QueuedCall.last
      enqueued_call.schedule_id.should eq(schedule.id)
      enqueued_call.project_id.should eq(project.id)
      enqueued_call.not_before.should eq(not_before + 1.day)

      response.should be_redirect
    end

    it 'should enqueue a call not before specific date with a timezone' do
      not_before = DateTime.new(2012, 1, 1, 4, 0, 0)
      expected_not_before = DateTime.parse "2012-01-02 10:00:00 GMT-3"

      schedule.time_from = '10:00'
      schedule.time_to = '18:00'
      schedule.save!

      BrokerClient.should_receive(:notify_call_queued).with(channel.id, expected_not_before)

      expect {
        post :enqueue_call, :id => project.id, :addresses => "1", :channel_id => channel.id, :schedule_id => schedule.id, :call_flow_id => call_flow.id, :not_before_date => '2012-01-01 4:00:00', :not_before => true, :time_zone => 'Buenos Aires'
      }.to change(QueuedCall, :count).by(1)

      enqueued_call = QueuedCall.last
      enqueued_call.schedule_id.should eq(schedule.id)
      enqueued_call.project_id.should eq(project.id)
      enqueued_call.not_before.should eq(expected_not_before)

      response.should be_redirect
    end

    it 'should enqueue multiple calls' do
      expect {
        post :enqueue_call, :id => project.id, :addresses => "0\n1\n2", :channel_id => channel.id, :schedule_id => schedule.id, :call_flow_id => call_flow.id
      }.to change(QueuedCall, :count).by(3)
      response.should be_redirect

      actual = QueuedCall.all
      [0,1,2].each do |num|
        actual[num].address.should eq(num.to_s)
      end
    end

    it 'should fail if there is no call flow' do
      expect {
        post :enqueue_call, :id => project.id, :addresses => "1", :channel_id => channel.id, :schedule_id => schedule.id
      }.to_not change(QueuedCall, :count).by(1)
      response.should be_redirect
      flash[:error].should eq('You need to select a Call Flow')
    end

    it 'should not enqueue multiple calls to the same number' do
      expect {
        post :enqueue_call, :id => project.id, :addresses => "0\n0\n0", :channel_id => channel.id, :schedule_id => schedule.id, :call_flow_id => call_flow.id
      }.to change(QueuedCall, :count).by(1)
      response.should be_redirect
    end

    context "contact with multiple numbers" do
      before(:each) do
        @contact = project.contacts.new
        @contact.addresses.build address: '1'
        @contact.addresses.build address: '2'
        @contact.save!
      end

      it 'should not enqueue multiple calls to the same contact' do
        expect {
          post :enqueue_call, :id => project.id, :addresses => "1\n2", :channel_id => channel.id, :schedule_id => schedule.id, :call_flow_id => call_flow.id
        }.to change(QueuedCall, :count).by(1)
        response.should be_redirect
      end

      it "should enqueue a call to a contact's first number" do
        expect {
          post :enqueue_call, :id => project.id, :addresses => "2", :channel_id => channel.id, :schedule_id => schedule.id, :call_flow_id => call_flow.id
        }.to change(QueuedCall, :count).by(1)
        response.should be_redirect

        QueuedCall.last.address.should eq('1')
      end
    end
  end
end
