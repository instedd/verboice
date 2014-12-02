require "spec_helper"

describe Jobs::HubJob do
  it "posts results to hub" do
    HubClient.current "token" => "some_token"

    stub_request(:post, "https://hub.instedd.org/callback/verboice/call?token=some_token").
         with(:body => %({"a":"b","c":"d"}),
              :headers => {'Content-Type'=>'application/json'}).
         to_return(:status => 200, :body => "", :headers => {})

    job = Jobs::HubJob.new(a: "b", c: "d")
    job.perform
  end
end
