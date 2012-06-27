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

describe Api::LogsController do
  include Devise::TestHelpers

  describe 'list' do

    before(:each) do
      @call = CallLog.make
      @other_call = CallLog.make
      @entry1 = CallLogEntry.make :call => @call
      @entry2 = CallLogEntry.make :call => @call
      @entry3 = CallLogEntry.make :call => @call
      @account = @call.account
      sign_in @account
    end

    it "should list all entries" do
      get :list, :call_id => @call.id, :format => :csv
      assigns(:log).should eq(@call)
      assigns(:entries).should eq([@entry1, @entry2, @entry3])
    end

    it "should list entries after some entry id" do
      get :list, :call_id => @call.id, :format => :csv, :after => @entry1.id
      assigns(:log).should eq(@call)
      assigns(:entries).should eq([@entry2, @entry3])
    end

    it "should not list entries of other account" do
      get :list, :call_id => @other_call.id, :format => :csv
      assigns(:log).should be_nil
      assigns(:entries).should be_nil
      response.response_code.should eq(404)
    end

  end

end
