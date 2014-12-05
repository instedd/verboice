require "spec_helper"

describe Jobs::HubJob do
  it "posts results to hub" do
    HubClient.current "token" => "some_token"

    base_url = HubClient.current.config.url
    connector_guid = HubClient.current.config.connector_guid

    stub_request(:post, "#{base_url}/api/notify/connectors/#{connector_guid}/projects/1/call_flows/2/$events/call_finished").
         with(:body => "{\"project_id\":1,\"call_flow_id\":2}",
              :headers => {'Content-Type'=>'application/json'}).
         to_return(:status => 200, :body => "", :headers => {})

    job = Jobs::HubJob.new(project_id: 1, call_flow_id: 2)
    job.perform
  end
end
