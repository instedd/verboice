require 'test_helper'

class ApplicationTest < ActiveSupport::TestCase
  context "validations" do
    setup { Application.make }

    should belong_to(:account)
    should have_many(:call_logs)

    should validate_presence_of(:name)
    should validate_uniqueness_of(:name).scoped_to(:account_id)
  end

  context "callbacks" do
    should "sets name to callback url if name is empty" do
      app = Application.make :name => nil, :callback_url => 'foo'
      assert_equal app.callback_url, app.name
    end

    should "keeps name if name set" do
      app = Application.make :name => 'bar', :callback_url => 'foo'
      assert_equal 'bar', app.name
    end

    should "saves flow in json" do
      app = Application.make_unsaved
      app.flow = [:play => 'foo']
      app.save!

      app.reload
      assert_equal [:play => 'foo'], app.flow
    end
  end

  context "commands" do
    setup do
      @app = Application.make_unsaved
    end

    should "commands is flow when present" do
      @app.flow = [:answer]
      assert_equal @app.flow, @app.commands
    end

    should "commands when callback url is present" do
      @app.callback_url = 'http://example.com'
      assert_equal [:answer, {:callback => @app.callback_url}], @app.commands
    end
  end

  context "call" do
    setup do
      @app = Application.make
    end

    should "call ok" do
      PbxClient.expects(:call).with do |address, app_id, call_log_id|
        @the_call_log_id = call_log_id
        address == 'foo' && app_id == @app.id
      end

      call_log = @app.call 'foo'
      assert_equal @the_call_log_id, call_log.id
      assert_equal :active, call_log.state
    end

    should "call raises" do
      PbxClient.expects(:call).with do |address, app_id, call_log_id|
        @the_call_log_id = call_log_id
        address == 'foo' && app_id == @app.id
      end.raises("Oh no!")

      call_log = @app.call 'foo'
      assert_equal @the_call_log_id, call_log.id
      assert_equal :failed, call_log.state
    end
  end
end
