require 'test_helper'

class AccountTest < ActiveSupport::TestCase
  test "call application" do
    account = Account.make
    app = account.applications.make

    seq = sequence('seq')
    account.applications.expects(:find).with(app.id).returns(app).in_sequence(seq)
    app.expects(:call).with('1234').in_sequence(seq)

    account.call :application => app.id, :address => '1234'
  end

  test "call callback finds or creates application" do
    account = Account.make
    app = mock('app')

    seq = sequence('seq')
    account.applications.expects(:find_or_create_by_callback_url).with('callback').returns(app).in_sequence(seq)
    app.expects(:call).with('1234').in_sequence(seq)

    account.call :callback => 'callback', :address => '1234'
  end
end
