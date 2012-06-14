require 'spec_helper'

describe OauthController do
  include Devise::TestHelpers

  before(:each) do
    @account = Account.make
    sign_in @account
  end

  describe "on google callback" do

    let!(:mock_auth_strategy) do
      double('auth_strategy')
    end

    let!(:mock_client) do
      mock_client = double('oauth2_google_client')
      OAuth2::Client.should_receive(:google).and_return(mock_client)
      mock_client.stub(:auth_code).and_return(mock_auth_strategy)
      mock_client
    end

    describe "successful" do

      before(:each) do
        Timecop.freeze(DateTime.new(2011,1,1,8,0,0))
        mock_auth_strategy.should_receive(:get_token).with("SAMPLECODE", anything)
          .and_return({:access_token => "ACCESS", :refresh_token => "REFRESH", :expires_in => '3600'})
      end

      after(:each) do
        Timecop.return
      end

      it "should create oauth token" do
        expect {
          get :google_callback, :code => "SAMPLECODE"
        }.to change(OAuthToken, :count).by(1)

        token = OAuthToken.first
        token.account_id.should eq(@account.id)
        token.access_token.should eq("ACCESS")
        token.refresh_token.should eq("REFRESH")
        token.expires_at.should eq(DateTime.new(2011,1,1,9,0,0))

        @account.google_oauth_token.should eq(token)
      end

      it "should update existing oauth token" do
        old_token = OAuthToken.make :access_token => "OLD_ACCESS", :account => @account
        @account.reload.google_oauth_token.should eq(old_token)

        get :google_callback, :code => "SAMPLECODE"

        OAuthToken.count.should eq(1)

        token = OAuthToken.first
        token.account_id.should eq(@account.id)
        token.access_token.should eq("ACCESS")
        token.refresh_token.should eq("REFRESH")
        token.expires_at.should eq(DateTime.new(2011,1,1,9,0,0))

        @account.reload.google_oauth_token.should eq(token)
      end


    end

  end


end
