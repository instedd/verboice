require 'spec_helper'

describe Account do
  it { should have_many(:applications) }
  it { should have_many(:channels) }
  it { should have_many(:call_logs) }
end
