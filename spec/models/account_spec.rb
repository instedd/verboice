require 'spec_helper'

describe Account do
  it { should have_many(:projects) }
  it { should have_many(:channels) }
  it { should have_many(:call_logs) }
  it { should have_many(:schedules) }
end
