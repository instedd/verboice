require 'spec_helper'

describe ScheduledCall do
  it { should belong_to(:project) }
  it { should belong_to(:call_flow) }
  it { should belong_to(:channel) }
  it { should belong_to(:schedule) }

  it { should validate_presence_of(:name) }
  it { should validate_presence_of(:project) }
  it { should validate_presence_of(:call_flow) }
  it { should validate_presence_of(:channel) }
  it { should validate_presence_of(:schedule) }
  it { should validate_presence_of(:frequency) }
  it { should validate_presence_of(:time_zone) }
end
