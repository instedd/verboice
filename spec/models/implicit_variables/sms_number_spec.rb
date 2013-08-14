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

module ImplicitVariables
  describe SmsNumber do

    let(:contact) { Contact.make }

    it "should tell key" do
      SmsNumber.key.should eq('sms_number')
    end

    it "should default to the contact's address" do
      SmsNumber.new(contact).value.should eq(contact.first_address)
    end

    it "should return persisted variable value if persisted" do
      contact.persisted_variables.create! :implicit_key => SmsNumber.key, :value => '456'
      SmsNumber.new(contact).value.should eq('456')
    end

    it "should return nil when use default is false" do
      SmsNumber.new(contact).value(false).should be_nil
    end

  end
end
