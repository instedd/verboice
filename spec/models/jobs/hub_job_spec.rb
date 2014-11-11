require "spec_helper"

describe Jobs::HubJob do
  it "posts results to hub" do
    project = Project.make
    contact = project.contacts.make
    address = contact.addresses.first.address
    call_log = project.call_logs.make address: address

    project_var = project.project_variables.make name: "var1"
    persisted_var = PersistedVariable.make contact_id: contact.id, project_variable_id: project_var.id, value: "value1"

    hub_config = Rails.configuration.verboice_configuration[:hub]
    hub_url = hub_config[:url]
    hub_token = hub_config[:token]

    stub_request(:post, "#{hub_url}?token=#{hub_token}").
         with(:body => "{\"project_id\":#{call_log.project_id},\"call_flow_id\":#{call_log.call_flow_id},\"address\":\"#{address}\",\"vars\":{\"var1\":\"value1\"}}").
         to_return(:status => 200, :body => "", :headers => {})

    job = Jobs::HubJob.new(call_log.id)
    job.perform
  end
end
