require 'spec_helper'

describe Schedule do
  context "validations" do
    subject { Schedule.make }

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

    context "next available time" do

      def t(x)
        Time.parse(x)
      end

      it "returns same value if range is not set" do
        subject.next_available_time(t '2012-05-05T12:34:56').should == t('2012-05-05T12:34:56')
      end

      context "with range during same day" do
        before(:each) do
          subject.time_from = '10:00'
          subject.time_to = '14:00'
        end

        it "returns same value if it falls inside range" do
          subject.next_available_time(t '2012-05-05T12:00:00Z').should == t('2012-05-05T12:00:00Z')
        end

        it "moves time forward if it falls behind the beginning" do
          subject.next_available_time(t '2012-05-05T08:00:00Z').should == t('2012-05-05T10:00:00Z')
        end

        it "moves time to next day if it falls after the end" do
          subject.next_available_time(t '2012-05-05T15:00:00Z').should == t('2012-05-06T10:00:00Z')
        end
      end

      context "with range ending next day" do
        before(:each) do
          subject.time_from = '18:00'
          subject.time_to = '05:00'
        end

        it "returns same value if it falls inside range after midnight" do
          subject.next_available_time(t '2012-05-05T02:00:00Z').should == t('2012-05-05T02:00:00Z')
        end

        it "returns same value if it falls inside range before midnight" do
          subject.next_available_time(t '2012-05-05T20:00:00Z').should == t('2012-05-05T20:00:00Z')
        end

        it "moves time forward if it falls outside the range" do
          subject.next_available_time(t '2012-05-05T10:00:00Z').should == t('2012-05-05T18:00:00Z')
        end
      end

      context "with weekdays" do
        before :each do
          subject.weekdays = '0,2,5'
        end

        it "returns same day if current day is in weekdays" do
          # tuesday
          subject.next_available_time(t '2012-05-01T00:00:00Z').should == t('2012-05-01T00:00:00Z')
        end

        it "returns next day in same week if current day is between weekdays" do
          # wednesday
          subject.next_available_time(t '2012-05-02T00:00:00Z').should == t('2012-05-04T00:00:00Z')
          # thursday
          subject.next_available_time(t '2012-05-03T00:00:00Z').should == t('2012-05-04T00:00:00Z')
        end

        it "returns next day in next week if current day is after weekdays" do
          # saturday
          subject.next_available_time(t '2012-05-05T00:00:00Z').should == t('2012-05-06T00:00:00Z')
        end
      end

      context 'with time zone GMT-3' do
        before :each do
          subject.time_zone = 'Buenos Aires'
        end

        it "returns same value if range is not set" do
          subject.next_available_time(t '2012-05-05 12:34:56 UTC').should == t('2012-05-05 12:34:56 UTC')
        end

        context "and range during same day" do
          before(:each) do
            subject.time_from = '10:00'
            subject.time_to = '14:00'
          end

          it "returns same value if it falls inside range" do
            subject.next_available_time(t '2012-05-05T16:00:00Z').should == t('2012-05-05 16:00:00 UTC')
          end

          it "moves time forward if it falls behind the beginning" do
            subject.next_available_time(t '2012-05-05T11:00:00Z').should == t('2012-05-05 13:00:00 UTC')
          end

          it "moves time to next day if it falls after the end" do
            subject.next_available_time(t '2012-05-05T18:00:00Z').should == t('2012-05-06 13:00:00 UTC')
          end
        end


      end
    end

  end
end
