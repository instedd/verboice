require 'spec_helper'

describe CallQueue do
  context "validations" do
    before(:each) { CallQueue.make }

    it { should belong_to(:account) }
    it { should validate_presence_of(:account) }
    it { should validate_presence_of(:name) }
  end
end
