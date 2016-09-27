require 'spec_helper'

describe "calls", integration: true do

  self.use_transactional_fixtures = false

  before(:all) do
    @account = Account.make
    @channel  = Channels::Custom.create! name: "local",  account: @account, dial_string: "Local/{number}@verboice-integration"
    @channel2 = Channels::Custom.create! name: "local2", account: @account, dial_string: "Local/{number}@verboice-integration"
    @project = @account.projects.create! name: "Testing", languages: [{
      "language" => "en",
      "voice" => TTS::SystemSynthesizer.instance.voices["en"].first[:id]
    }], default_language: "en", tts_engine: "built-in"
  end

  after(:all) do
    @account.destroy
  end

  it "captures a digit" do
    digit = 7
    resource = @project.resources.make_with_text(text: "Enter a key")
    call_flow = @project.call_flows.make user_flow: [{
      'id' => 1,
      'root' => true,
      'type' => 'capture',
      'name' => 'Capture',
      'store' => 'number',
      'instructions_resource' => { "guid" => resource.guid },
      'invalid_resource' => {},
      'valid_values' => '0-9',
      'finish_on_key' => '#',
      'min_input_length' => 1,
      'max_input_length' => 1,
      'timeout' => 10
    }]

    call_log = make_interactive_call(call_flow, @channel, "9991000") do |session|
      session.send_dtmf digit
    end

    wait_call(call_log)

    address = @project.contact_addresses.where(address: "9991000").first
    address.should_not be_nil

    variable = @project.project_variables.where(name: "number").first
    variable.should_not be_nil

    persisted_variable = address.contact.persisted_variables.where(project_variable_id: variable.id).first
    persisted_variable.should_not be_nil
    persisted_variable.value.should eq(digit.to_s)
  end

  it "listens to a specific digit" do
    resource = @project.resources.make_with_recording(file: "spec/fixtures/dtmf/9b.wav")
    call_flow = @project.call_flows.make user_flow: [{
      'id' => 1,
      'root' => true,
      'type' => 'play',
      'name' => 'Play',
      'resource' => { "guid" => resource.guid }
    }]

    call_log = make_interactive_call(call_flow, @channel, "9991000") do |session|
      puts session.wait_for_digit
    end

    wait_call(call_log)
  end

end
