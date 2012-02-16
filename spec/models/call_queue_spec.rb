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

    it "convert time to string" do
      subject.time_from = Time.parse '10:03'
      subject.time_to = Time.parse '10:03'
      subject.time_from_str.should == '10:03'
      subject.time_to_str.should == '10:03'
    end

    it "converts nil time to empty string" do
      subject.time_from_str.should == ''
      subject.time_to_str.should == ''
    end

    it "parses time from string" do
      subject.time_from_str = '10:03'
      subject.time_from.as_seconds.should == Time.parse('10:03').as_seconds
    end

    it "parses empty time from string" do
      subject.time_from_str = ''
      subject.time_from.should be_nil
    end

  end
end
