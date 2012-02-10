require 'spec_helper'

describe CallQueue do
  context "validations" do
    subject { CallQueue.make }

    it { should belong_to(:account) }
    it { should validate_presence_of(:account) }
    it { should validate_presence_of(:name) }
    it { should_not allow_value("ABC").for(:retries) }
    it { should allow_value("5").for(:retries) }
    it { should allow_value("1,2,3").for(:retries) }
    it { should allow_value("1.5").for(:retries) }
    it { should_not allow_value("1,,2").for(:retries) }


  end
end
