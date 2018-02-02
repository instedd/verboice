require 'spec_helper'

describe Commands::NuntiumCommand do
  it 'should serialize parameters with channel_id if present' do
    command = Commands::NuntiumCommand.new('resource_guid', 7, :caller)
    expect(command.serialize_parameters).to eq({
      expr: nil,
      resource_guid: 'resource_guid',
      rcpt_type: :caller,
      channel_id: 7
    })
  end

  it 'should serialize parameters without channel_id if channel is missing' do
    command = Commands::NuntiumCommand.new('resource_guid', nil, :caller)
    expect(command.serialize_parameters).to eq({
      expr: nil,
      resource_guid: 'resource_guid',
      rcpt_type: :caller
    })
  end
end
