require 'spec_helper'

describe Application do
  context "validations" do
    before(:each) { Application.make }

    it { should belong_to(:account) }
    it { should have_many(:call_logs) }

    it { should validate_presence_of(:name) }
    it { should validate_uniqueness_of(:name).scoped_to(:account_id) }
  end

  context "callbacks" do
    it "sets name to callback url if name is empty" do
      app = Application.make :name => nil, :callback_url => 'foo'
      app.name.should == app.callback_url
    end

    it "keeps name if name set" do
      app = Application.make :name => 'bar', :callback_url => 'foo'
      app.name.should == 'bar'
    end

    it "saves flow in json" do
      app = Application.make_unsaved
      app.flow = [:play_url => 'foo']
      app.save!

      app.reload
      app.flow.should == [:play_url => 'foo']
    end
  end

  context "commands" do
    before(:each) do
      @app = Application.make_unsaved
    end

    it "commands is flow when present" do
      @app.flow = [:answer]
      @app.commands.should == @app.flow
    end

    it "commands when callback url is present" do
      @app.callback_url = 'http://example.com'
      @app.commands.should eq([:answer, {:callback => @app.callback_url}])
    end
  end
end
