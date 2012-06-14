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

describe RecordedAudiosController do
  include Devise::TestHelpers

  before(:each) do
    sign_in account
  end

  let!(:account) { Account.make }
  let!(:project) { Project.make account: account }
  let!(:contact) { Contact.make project: project }
  let!(:other_contact) { Contact.make }
  let!(:call_log) { CallLog.make account: account }
  let!(:recorded_audio) { RecordedAudio.make contact: contact }
  let!(:other_recorded_audio) { RecordedAudio.make contact: other_contact }

  describe "GET index" do
    it "assigns all recorded_audios as @recorded_audios" do
      get :index, {:contact_id => contact.to_param}
      assigns(:recorded_audios).should eq([recorded_audio])
    end
  end

  describe "GET show" do
    it "assigns the requested recorded_audio as @recorded_audio" do
      get :show, {:id => recorded_audio.to_param}
      assigns(:recorded_audio).should eq(recorded_audio)
    end

    it "fails if the requested recorded_audio is not in current account contacts" do
      expect {
        get :show, {:id => other_recorded_audio.to_param}
      }.should raise_error
      assigns(:recorded_audio).should be_nil
    end
  end

  describe "DELETE destroy" do
    it "destroys the requested recorded_audio" do
      expect {
        delete :destroy, {:id => recorded_audio.to_param}
      }.to change(RecordedAudio, :count).by(-1)
    end

    it "redirects to the recorded_audios list" do
      delete :destroy, {:id => recorded_audio.to_param}
      response.should redirect_to(contact_recorded_audios_path(contact))
    end

    it "fails if the requested recorded_audio is not in current account contacts" do
      expect {
        delete :destroy, {:id => other_recorded_audio.to_param}
      }.should raise_error
      assigns(:recorded_audio).should be_nil
      RecordedAudio.find(other_recorded_audio.id).should eq(other_recorded_audio)
    end
  end

end
