require 'spec_helper'

describe LocalizedResourcesController do
  include Devise::TestHelpers

  before(:each) do
    @account = Account.make
    @project = @account.projects.make
    @resource = @project.resources.make

    sign_in @account
  end

  describe "recording" do

    before(:each) do
      @localized_resource = RecordLocalizedResource.make :resource => @resource
    end

    describe "POST save_recording" do

      it "should save recording" do
        request.env['RAW_POST_DATA'] = 'some recording'
        post :save_recording, {:project_id => @project.id, :resource_id => @resource.id, :id => @localized_resource.id}
        @localized_resource.reload.recorded_audio.should eq('some recording')
      end

      it "should succeed" do
        post :save_recording, {:project_id => @project.id, :resource_id => @resource.id, :id => @localized_resource.id}
        response.should be_ok
      end

    end

    describe "GET play_recording" do

      it "should return audio" do
        controller.should_receive(:send_data).with(@localized_resource.recorded_audio).and_return{controller.render :nothing => true}
        get :play_recording, {:project_id => @project.id, :resource_id => @resource.id, :id => @localized_resource.id}
      end

    end

  end

  describe "file" do

    before(:each) do
      @localized_resource = UploadLocalizedResource.make :resource => @resource
    end

    describe "POST save_file" do

      it "should save file" do
        request.env['RAW_POST_DATA'] = 'some file'
        post :save_file, {:project_id => @project.id, :resource_id => @resource.id, :id => @localized_resource.id}
        @localized_resource.reload.uploaded_audio.should eq('some file')
      end

      it "should save filename" do
        post :save_file, {:project_id => @project.id, :resource_id => @resource.id, :id => @localized_resource.id, :filename => 'filename.foo'}
        @localized_resource.reload.filename.should eq('filename.foo')
      end

      it "should succeed" do
        post :save_recording, {:project_id => @project.id, :resource_id => @resource.id, :id => @localized_resource.id}
        response.should be_ok
      end

    end

    describe "GET play_file" do

      it "should return file" do
        controller.should_receive(:send_data).with(@localized_resource.uploaded_audio, :filename => @localized_resource.filename).and_return{controller.render :nothing => true}
        get :play_file, {:project_id => @project.id, :resource_id => @resource.id, :id => @localized_resource.id}
      end

    end
  end

end