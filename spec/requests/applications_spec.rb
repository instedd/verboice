require 'spec_helper'

describe "Applications" do
  include ApplicationConfigHelpers

  let(:account) { Account.make(:password => "secret") }
  let(:new_application) { Application.last }
  let(:application) { Application.make(:account => account) }

  def fill_in_callback_fields
    with_callback_url_accessors do |accessor, field_type|
      field_id = "application_#{accessor}"
      fill_in field_id, :with => accessor.to_s
      find("##{field_id}")[:type].should == field_type.to_s
    end
  end

  shared_examples_for "saving the configuration" do
    it "should save the configuration" do
      with_callback_url_accessors do |accessor|
        reference_application.send(accessor).should == accessor.to_s
      end
    end
  end

  context "given I am logged in" do
    before do
      visit new_account_session_path
      fill_in "account_email", :with => account.email
      fill_in "account_password", :with => "secret"
      click_button "Log In"
    end

    context "and I am editing an existing application" do
      before do
        visit edit_application_path(application)
      end

      context "and I update the application supplying a username and password" do
        before do
          fill_in_callback_fields
          click_button "Update Application"
        end

        it_should_behave_like "saving the configuration" do
          let(:reference_application) { application.reload }
        end
      end
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
          before do
            fill_in_callback_fields
            click_button "Create Application"
          end

          it_should_behave_like "saving the configuration" do
            let(:reference_application) { new_application }
          end
        end
      end
    end
  end
end
