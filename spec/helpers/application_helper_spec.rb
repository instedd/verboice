# Copyright (C) 2010-2012, InSTEDD
# 
# This file is part of Verboice.
# 
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

require 'spec_helper'

describe ApplicationHelper do
  describe "#with_callback_url_fields" do

    shared_examples_for "callback url fields" do |type|
      it "should return the correct field" do
        type = type.to_s << "_" if type

        helper.with_callback_url_fields(type) do |field, field_type|
          field.should be_a(Symbol)
          parsed_field = field.to_s
          parsed_field.should =~ /^#{type}callback/
          parsed_field.should =~ /_(url_user|url_password|url)$/
          field_type_assertion = field =~ /password/ ? :password_field : :text_field
          field_type.should == field_type_assertion
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
