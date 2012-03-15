require 'spec_helper'

describe "Applications" do

  let(:account) { Account.make(:password => "secret") }
  let(:application) { Application.last }

  context "given I am logged in" do
    before do
      visit new_account_session_path
      fill_in "account_email", :with => account.email
      fill_in "account_password", :with => "secret"
      click_button "Log In"
    end

    context "and on the new Application page" do
      before do
        visit new_application_path
      end

      context "and I create a new application with a callback url" do
        before do
          fill_in "application_name", :with => "Test"
          select "Use a callback URL", :from => "application_mode"
        end

        context "I fill in the user and password fields" do
          include ApplicationConfigHelpers

          before do
            with_callback_url_accessors do |accessor|
              fill_in "application_#{accessor}", :with => accessor.to_s
            end

            click_button "Create Application"
          end

          it "should take me to the page" do
            with_callback_url_accessors do |accessor|
              application.send(accessor).should == accessor.to_s
            end
          end
        end
      end
    end
  end
end
