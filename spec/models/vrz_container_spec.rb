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

describe VrzContainer do

  before(:each) do

    @resource = Resource.make name: 'resource name'
    @project = @resource.project

    @call_flow = CallFlow.make user_flow: [
      {
        'id' => 1,
        'root' => 1,
        'type' => 'play',
        'name' => 'Play number one',
        'resource' => {
          "guid" => @resource.guid
        }
      }
    ], project: @project, name: 'call flow name'

    @external_service = ExternalService.make project: @project, name: 'external service name'
    @external_step = ExternalServiceStep.make external_service: @external_service, name: 'external step name'
    CallFlowExternalService.make call_flow: @call_flow, external_service: @external_service

    @localized_resource = UploadLocalizedResource.make resource: @resource, language: 'en', audio: 'some audio'

    @vrz_container = VrzContainer.for @call_flow
  end

  it 'exports' do

    path = '/path/to/zip'

    # Mock zip open
    zip_block = nil
    expect(Zip::ZipOutputStream).to receive(:open) do |the_path, &block|
      zip_block = block
      expect(the_path).to eq(path)
    end

    # Mock zip stream
    stream = double('stream')
    expect(stream).to receive(:put_next_entry).with('workflow.yml')
    expect(stream).to receive(:print).with(@call_flow.user_flow.to_yaml)

    expect(stream).to receive(:put_next_entry).with("Service #{@external_service.guid}.yml")
    expect(stream).to receive(:print).with(@external_service.attributes.tap do |attributes|
      attributes.delete 'id'
      attributes.delete 'project_id'
    end.to_yaml)

    expect(stream).to receive(:put_next_entry).with("Step #{@external_step.guid}.yml")
    expect(stream).to receive(:print).with(@external_step.attributes.tap do |attributes|
      attributes.delete 'id'
      attributes.delete 'external_service_id'
      attributes['external_service_guid'] = @external_step.external_service.guid
    end.to_yaml)

    expect(stream).to receive(:put_next_entry).with("localized_resource #{@resource.guid} - #{@localized_resource.language} - #{@localized_resource.guid}.yml")
    expect(stream).to receive(:print).with(@localized_resource.attributes.tap do |attributes|
      attributes.delete 'audio'
      attributes.delete 'id'
      attributes.delete 'resource_id'
      attributes['resource_guid'] = @localized_resource.resource.guid
    end.to_yaml)

    expect(stream).to receive(:put_next_entry).with("resource_audio #{@resource.guid} - #{@localized_resource.language} - #{@localized_resource.guid}.wav")
    expect(stream).to receive(:print).with(@localized_resource.audio)

    expect(stream).to receive(:put_next_entry).with("resource #{@resource.guid}.yml")
    expect(stream).to receive(:print).with(@resource.attributes.tap do |attributes|
      attributes.delete 'id'
      attributes.delete 'project_id'
    end.to_yaml)

    @vrz_container.export path
    zip_block.call stream
  end

  it 'imports when resources and external services doesnt exist' do

    in_temp_dir do |path|
      zip_path = File.join(path, 'zip_file.zip')

      @vrz_container.export(zip_path)

      CallFlow.first.tap { |f| f.user_flow = nil }.save!
      ExternalService.delete_all
      ExternalServiceStep.delete_all
      Resource.delete_all
      UploadLocalizedResource.delete_all

      @vrz_container.import zip_path

    end

    expect(CallFlow.count).to eq(1)
    expect(CallFlow.first.user_flow).to eq(@call_flow.user_flow)

    expect(Resource.count).to eq(1)
    expect(Resource.first.project).to eq(@project)
    expect(Resource.first.guid).to eq(@resource.guid)

    expect(LocalizedResource.count).to eq(1)
    expect(LocalizedResource.first.guid).to eq(@localized_resource.guid)
    expect(LocalizedResource.first.resource.attributes.except('id', 'created_at', 'updated_at')).to eq(@resource.attributes.except('id', 'created_at', 'updated_at'))
    expect(LocalizedResource.first.audio).to eq(@localized_resource.audio)
    expect(LocalizedResource.first.type).to eq("UploadLocalizedResource")
    expect(LocalizedResource.first.attributes.except('id', 'created_at', 'updated_at', 'resource_id')).to eq(@localized_resource.attributes.except('id', 'created_at', 'updated_at', 'resource_id'))

    expect(ExternalService.count).to eq(1)
    expect(ExternalService.first.guid).to eq(@external_service.guid)
    expect(ExternalService.first.project).to eq(@project)

    expect(ExternalServiceStep.count).to eq(1)
    expect(ExternalServiceStep.first.guid).to eq(@external_step.guid)
    expect(ExternalServiceStep.first.external_service.attributes.except('id', 'created_at', 'updated_at', 'external_service_id')).to eq(@external_service.attributes.except('id', 'created_at', 'updated_at', 'external_service_id'))
  end

  it 'imports when resources and external services do exist' do

    in_temp_dir do |path|
      zip_path = File.join(path, 'zip_file.zip')

      @vrz_container.export(zip_path)

      CallFlow.first.tap do|f|
        f.user_flow = nil
        f.name = 'other name'
      end.save!
      ExternalService.first.tap {|f| f.name = 'new external service name' }.save!
      ExternalServiceStep.first.tap {|f| f.name = 'new external service step name' }.save!
      Resource.first.tap {|f| f.name = 'asdfgr321' }.save!
      UploadLocalizedResource.first.tap do|f|
        f.audio = 'some other audio'
        f.language = 'es'
      end.save!

      @vrz_container.import zip_path

    end

    expect(CallFlow.count).to eq(1)
    expect(CallFlow.first.user_flow).to eq(@call_flow.user_flow)

    expect(Resource.count).to eq(1)
    expect(Resource.first.project).to eq(@project)
    expect(Resource.first.guid).to eq(@resource.guid)

    expect(LocalizedResource.count).to eq(1)
    expect(LocalizedResource.first.guid).to eq(@localized_resource.guid)
    expect(LocalizedResource.first.resource.attributes.except('updated_at')).to eq(@resource.attributes.except('updated_at'))
    expect(LocalizedResource.first.audio).to eq(@localized_resource.audio)
    expect(LocalizedResource.first.type).to eq("UploadLocalizedResource")
    expect(LocalizedResource.first.attributes.except('updated_at')).to eq(@localized_resource.attributes.except('updated_at'))

    expect(ExternalService.count).to eq(1)
    expect(ExternalService.first.guid).to eq(@external_service.guid)
    expect(ExternalService.first.project).to eq(@project)
    expect(ExternalService.first.name).to eq('new external service name')

    expect(ExternalServiceStep.count).to eq(1)
    expect(ExternalServiceStep.first.guid).to eq(@external_step.guid)
    expect(ExternalServiceStep.first.name).to eq('new external service step name')
  end

  it 'doesnt overrides resources and external steps from another project' do

    in_temp_dir do |path|
      zip_path = File.join(path, 'zip_file.zip')

      @vrz_container.export(zip_path)

      CallFlow.first.tap do|f|
        f.user_flow = nil
        f.name = 'other name'
      end.save!
      ExternalService.first.tap {|f| f.name = 'new external service name' }.save!
      ExternalServiceStep.first.tap {|f| f.name = 'new external service step name' }.save!
      Resource.first.tap {|f| f.name = 'asdfgr321' }.save!
      UploadLocalizedResource.first.tap do|f|
        f.audio = 'some other audio'
        f.language = 'es'
      end.save!

      @second_call_flow = CallFlow.make

      VrzContainer.for(@second_call_flow).import zip_path

    end

    # p "project: #{@project.to_yaml}"
    # p "first resource project: #{LocalizedResource.first.project.to_yaml}"
    # p "last resource project: #{LocalizedResource.last.project.to_yaml}"
    # p "original resource project: #{@localized_resource.project.to_yaml}"

    expect(CallFlow.count).to eq(2)
    expect(CallFlow.first.user_flow).to eq(nil)
    expect(CallFlow.last.user_flow).to eq(@call_flow.user_flow)

    expect(Resource.count).to eq(2)
    expect(Resource.first.project).to eq(@project)
    expect(Resource.first.guid).to eq(@resource.guid)

    expect(Resource.last.project).to eq(@second_call_flow.project)
    expect(Resource.last.guid).to eq(@resource.guid)

    expect(LocalizedResource.count).to eq(2)
    expect(LocalizedResource.first.guid).to eq(@localized_resource.guid)
    expect(LocalizedResource.first.resource).to eq(@resource)
    expect(LocalizedResource.first.audio).to eq(@localized_resource.audio)
    expect(LocalizedResource.first.type).to eq("UploadLocalizedResource")
    expect(LocalizedResource.first).to eq(@localized_resource)
    expect(LocalizedResource.first.project).to eq(@project)

    expect(LocalizedResource.last.guid).to eq(@localized_resource.guid)
    expect(LocalizedResource.last.resource.guid).to eq(@resource.guid)
    expect(LocalizedResource.last.audio).to eq(@localized_resource.audio)
    expect(LocalizedResource.last.project).to eq(@second_call_flow.project)
    expect(LocalizedResource.last.type).to eq("UploadLocalizedResource")
    expect(LocalizedResource.last.attributes.except('id','updated_at', 'created_at', 'resource_id')).to eq(@localized_resource.attributes.except('id','updated_at', 'created_at', 'resource_id'))

    expect(ExternalService.count).to eq(2)
    expect(ExternalService.first.guid).to eq(@external_service.guid)
    expect(ExternalService.first.project).to eq(@project)
    expect(ExternalService.first.name).to eq('new external service name')

    expect(ExternalService.last.guid).to eq(@external_service.guid)
    expect(ExternalService.last.project).to eq(@second_call_flow.project)
    expect(ExternalService.last.name).to eq('external service name')

    expect(ExternalServiceStep.count).to eq(2)
    expect(ExternalServiceStep.first.guid).to eq(@external_step.guid)
    expect(ExternalServiceStep.first.name).to eq('new external service step name')
    expect(ExternalServiceStep.first.project).to eq(@project)

    expect(ExternalServiceStep.last.guid).to eq(@external_step.guid)
    expect(ExternalServiceStep.last.name).to eq('external step name')
    expect(ExternalServiceStep.last.project).to eq(@second_call_flow.project)
  end

  def in_temp_dir

    path = File.expand_path "#{Rails.root}/tmp/data/#{Time.now.to_i}#{rand(1000)}/"
    FileUtils.mkdir_p(path)

    yield(path)

  ensure
    FileUtils.rm_rf(path) if File.exist?(path)
  end

end
