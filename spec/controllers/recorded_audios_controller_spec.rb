require 'spec_helper'

describe RecordedAudiosController do
  include Devise::TestHelpers

  before(:each) do
    sign_in account
  end
  let!(:account) { Account.make }
  let!(:contact) { Contact.make account: account }
  let!(:call_log) { CallLog.make account: account }
  let!(:recorded_audio) { RecordedAudio.make contact: contact }

  describe "GET index" do
    it "assigns all recorded_audios as @recorded_audios" do
      get :index, {:contact_id => contact.to_param}
      assigns(:recorded_audios).should eq([recorded_audio])
    end
  end

  describe "GET show" do
    it "assigns the requested recorded_audio as @recorded_audio" do
      get :show, {:id => recorded_audio.to_param, :contact_id => contact.to_param}
      assigns(:recorded_audio).should eq(recorded_audio)
    end
  end

  describe "DELETE destroy" do
    it "destroys the requested recorded_audio" do
      expect {
        delete :destroy, {:id => recorded_audio.to_param, :contact_id => contact.to_param}
      }.to change(RecordedAudio, :count).by(-1)
    end

    it "redirects to the recorded_audios list" do
      delete :destroy, {:id => recorded_audio.to_param, :contact_id => contact.to_param}
      response.should redirect_to(contact_recorded_audios_path(contact))
    end
  end

end
