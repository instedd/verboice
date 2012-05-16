require 'spec_helper'

describe "Projects" do
  include ProjectConfigHelpers

  let(:account) { Account.make(:password => "secret") }
  let(:new_project) { Project.last }
  let(:project) { Project.make(:account => account) }

  def fill_in_callback_fields
    with_callback_url_accessors do |accessor, field_type|
      field_id = "project_#{accessor}"
      fill_in field_id, :with => accessor.to_s
      find("##{field_id}")[:type].should == field_type.to_s
    end
  end

  shared_examples_for "saving the configuration" do
    it "should save the configuration" do
      with_callback_url_accessors do |accessor|
        reference_project.send(accessor).should == accessor.to_s
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

    context "and I am editing an existing project" do
      before do
        visit edit_project_path(project)
      end

      context "and I update the project supplying a username and password" do
        before do
          fill_in_callback_fields
          click_button "Update"
        end

        it_should_behave_like "saving the configuration" do
          let(:reference_project) { project.reload }
        end
      end
    end

    context "and on the new Project page" do
      before do
        visit new_project_path
      end

      context "and I create a new project with a callback url" do
        before do
          fill_in "project_name", :with => "Test"
          select "Use an external application", :from => "project_mode"
        end

        context "I fill in the user and password fields" do
          before do
            fill_in_callback_fields
            click_button "Save"
          end

          it_should_behave_like "saving the configuration" do
            let(:reference_project) { new_project }
          end
        end
      end
    end
  end
end
