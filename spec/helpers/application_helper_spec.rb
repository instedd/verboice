require 'spec_helper'

describe ApplicationHelper do
  describe "#with_callback_url_fields" do

    shared_examples_for "callback url fields" do |type|
      it "should return the correct field" do
        type = type.to_s << "_" if type

        helper.with_callback_url_fields(type) do |field|
          field.should be_a(Symbol)
          parsed_field = field.to_s
          parsed_field.should =~ /^#{type}callback/
          parsed_field.should =~ /_(url_user|url_password|url)$/
        end
      end
    end

    context "passing ':status'" do
      it_should_behave_like "callback url fields", :status
    end

    context "passing no args" do
      it_should_behave_like "callback url fields"
    end
  end
end
