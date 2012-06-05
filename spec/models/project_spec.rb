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

describe Project do

  context "validations" do
    before(:each) { Project.make }

    it { should belong_to(:account) }
    it { should have_many(:call_logs) }

    it { should validate_presence_of(:name) }
    it { should validate_uniqueness_of(:name).scoped_to(:account_id) }
  end

  context "config" do
    include ProjectConfigHelpers

    it "should be encrypted" do
      subject.config = {:some => :config}
      subject.encrypted_config.should_not == {:some => :config}
    end

    it "should propertly saved the encrypted config to the db" do
      project = Project.make
      with_callback_url_accessors do |accessor|
        project.send("#{accessor}=", accessor.to_s)
        project.save
        subject.class.find(project.id).send(accessor).should == accessor.to_s
      end
    end

    it "should have accessors for all configuration" do
      with_callback_url_accessors do |accessor|
        subject.send("#{accessor}=", accessor)
        subject.send(accessor).should == accessor
      end
    end
  end
end
