require 'test_helper'

class AccountTest < ActiveSupport::TestCase
  should have_many(:applications)
  should have_many(:channels)
  should have_many(:call_logs)

  context "call" do
    setup do
      @account = Account.make
      @account.stubs(:applications => mock('applications'))
      @app = mock('application')
      @seq = sequence('seq')
    end

    should "call application" do
      @account.applications.expects(:find).with(987).returns(@app).in_sequence(@seq)
      @app.expects(:call).with('1234').in_sequence(@seq)

      @account.call :application => 987, :address => '1234'
    end

    should "call callback finds or creates application" do
      @account.applications.expects(:find_or_create_by_callback_url).with('callback').returns(@app).in_sequence(@seq)
      @app.expects(:call).with('1234').in_sequence(@seq)

      @account.call :callback => 'callback', :address => '1234'
    end
  end
end
