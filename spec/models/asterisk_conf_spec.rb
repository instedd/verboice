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

describe Asterisk::Conf do
  FileUtils.mkdir_p "#{Rails.root}/tmp"
  TmpFileName = "#{Rails.root}/tmp/sip.conf"

  before(:each) do
    FileUtils.cp "#{Rails.root}/spec/fixtures/sip.conf", TmpFileName
  end

  after(:each) do
    FileUtils.rm TmpFileName
  end

  it "remove section" do
    Asterisk::Conf.change TmpFileName do
      remove :gateway1
    end

    assert_result 'remove'
  end

  it "add section when not already present" do
    Asterisk::Conf.change TmpFileName do
      add :gateway2, :type => :peer, :context => :verboice, :allow => [:foo, :bar]
    end

    assert_result 'add_when_not_already_present'
  end

  it "add section when already present" do
    Asterisk::Conf.change TmpFileName do
      add :gateway1, :type => :peer, :context => :verboice, :allow => [:foo, :bar]
    end

    assert_result 'add_when_already_present'
  end

  it "add register to global section" do
    Asterisk::Conf.change TmpFileName do
      add_action :general, :register, 'xxx:yyy@zzz.com'
    end

    assert_result 'add_action'
  end

  it "add register to last section" do
    Asterisk::Conf.change TmpFileName do
      add_action :gateway3, :register, 'xxx:yyy@zzz.com'
    end

    assert_result 'add_last_action'
  end

  it "add register to global section already exists" do
    Asterisk::Conf.change TmpFileName do
      add_action :general, :register, 'foo:bar@host.com'
    end

    assert_result 'sip'
  end

  it "remove register from global section" do
    Asterisk::Conf.change TmpFileName do
      remove_action :general, :register, 'foo:bar@host.com'
    end

    assert_result 'remove_action'
  end

  it "add with template" do
    Asterisk::Conf.change TmpFileName do
      add :gateway2, :type => :peer, :context => :verboice, :allow => [:foo, :bar], :template => '!'
      add :gateway2_0, :host => :foo, :template => :gateway2
      add :gateway2_1, :host => :bar, :template => :gateway2
    end

    assert_result 'add_with_template'
  end

  def assert_result(file)
    result = `diff -U10 #{Rails.root}/spec/fixtures/#{file}.conf #{TmpFileName}`
    $?.exitstatus.should eq(0)
  end
end
