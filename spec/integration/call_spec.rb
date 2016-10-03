require 'spec_helper'
require 'socket'

describe "calls", integration: true do

  self.use_transactional_fixtures = false

  before(:all) do
    ActiveRecord::Base.logger.level = Logger::INFO
    Account.destroy_all
    sleep 1

    @account = Account.make
    @sip_channel = Channels::SipServer.create! name: "custom-sip", account: @account, number: 1234567890, password: "1234"
    @host_ip = Socket.ip_address_list.detect{ |intf| intf.ipv4? && !intf.ipv4_loopback? && !intf.ipv4_multicast? && intf.ipv4_private? }.ip_address

    config_verboice_asterisk(@host_ip)
    wait_for_channel_definition(@sip_channel.id)
    config_remote_asterisk(@host_ip, @sip_channel.id)

    @project = @account.projects.create! name: "Testing", languages: [{
      "language" => "en",
      "voice" => TTS::SystemSynthesizer.instance.voices["en"].first[:id]
    }], default_language: "en", tts_engine: "built-in"
  end

  after(:all) do
    @account.try(:destroy)
    unconfig_remote_asterisk
  end

  it "captures a digit" do
    Rails.logger.info("Capture digit test")
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

    @sip_channel.call_flow = call_flow
    @sip_channel.save!

    call_log = make_remote_interactive_call do |session|
      session.send_dtmf digit
    end

    wait_call(call_log)

    address = @project.contact_addresses.where(address: "verboice_#{@sip_channel.id}").first
    address.should_not be_nil

    variable = @project.project_variables.where(name: "number").first
    variable.should_not be_nil

    persisted_variable = address.contact.persisted_variables.where(project_variable_id: variable.id).first
    persisted_variable.should_not be_nil
    persisted_variable.value.should eq(digit.to_s)
  end

  it "captures multiple digits" do
    Rails.logger.info("Capture multiple digits test")
    number = 42
    resource = @project.resources.make_with_text(text: "Enter a key")
    call_flow = @project.call_flows.make user_flow: [{
      'id' => 1,
      'root' => true,
      'type' => 'capture',
      'name' => 'Capture',
      'store' => 'number',
      'instructions_resource' => { "guid" => resource.guid },
      'invalid_resource' => {},
      'valid_values' => '0-99999',
      'finish_on_key' => '#',
      'min_input_length' => 1,
      'max_input_length' => 4,
      'timeout' => 20
    }]

    @sip_channel.call_flow = call_flow
    @sip_channel.save!

    call_log = make_remote_interactive_call do |session|
      session.send_dtmf 4
      sleep 1
      session.send_dtmf 2
      sleep 1
      session.send_dtmf '#'
    end

    wait_call(call_log)

    address = @project.contact_addresses.where(address: "verboice_#{@sip_channel.id}").first
    address.should_not be_nil

    variable = @project.project_variables.where(name: "number").first
    variable.should_not be_nil

    persisted_variable = address.contact.persisted_variables.where(project_variable_id: variable.id).first
    persisted_variable.should_not be_nil
    persisted_variable.value.should eq(number.to_s)
  end

  it "listens to a specific digit" do
    Rails.logger.info("Listen to digit test")
    resource = @project.resources.make_with_recording(file: "spec/fixtures/dtmf/9.wav")
    call_flow = @project.call_flows.make user_flow: [{
      'id' => 1,
      'root' => true,
      'type' => 'play',
      'name' => 'Play',
      'resource' => { "guid" => resource.guid }
    }]

    @sip_channel.call_flow = call_flow
    @sip_channel.save!

    call_log = make_remote_interactive_call do |session|
      session.record_call "record-#{@sip_channel.id}", 10000
    end

    wait_call(call_log)
    assert_dtmf("asterisk-sounds/record-#{@sip_channel.id}.au", 9)
  end

  it "records a file" do
    Rails.logger.info("Record a file test")
    explanation_resource = @project.resources.make_with_text(text: "Record a message")
    confirmation_resource = @project.resources.make_with_text(text: "Confirmed")

    call_flow = @project.call_flows.make user_flow: [{
      "id"=>1,
      "name"=>"Record",
      "type"=>"record",
      "root"=>true,
      "timeout"=>"20",
      "stop_key"=>"#",
      "explanation_resource"=>{"guid"=>explanation_resource.guid},
      "confirmation_resource"=>{"guid"=>confirmation_resource.guid}
    }]

    @sip_channel.call_flow = call_flow
    @sip_channel.save!

    dtmf_file = File.join(Rails.root, "spec/support/etc/asterisk-sounds/verboice/dtmf9.gsm")
    unless File.exists?(dtmf_file)
      FileUtils.copy(File.join(Rails.root, "spec/fixtures/dtmf/9.gsm"), dtmf_file)
    end

    call_log = make_remote_interactive_call do |session|
      sleep 3
      session.play_file "verboice/dtmf9"
      sleep 3
      session.send_dtmf "#"
      sleep 5
      session.hangup
    end

    wait_call(call_log)

    audio = RecordedAudio.where(call_log_id: call_log.id).first
    audio.should_not be_nil

    assert_dtmf("asterisk-recordings/#{call_log.id}/results/#{audio.key}.wav", 9)
  end

end
