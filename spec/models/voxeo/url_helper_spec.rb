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

describe Voxeo::UrlHelper do

  before(:each) do
    Rails.configuration.voxeo_configuration[:http_url_options] = {:host => "serverhost.com", :port => 1234}
  end

  it 'should return audio url for a key' do
    Voxeo::UrlHelper.audio_url(1234, {:foo => 'bar'}).should eq('http://serverhost.com:1234/audio/1234?foo=bar')
  end

  it 'should return callback url' do
     Voxeo::UrlHelper.callback_url.should eq('http://serverhost.com:1234/')
  end

  it 'should not use port when there is no port in config' do
    Rails.configuration.voxeo_configuration[:http_url_options][:port] = ''
    Voxeo::UrlHelper.url.should eq('http://serverhost.com/')
  end

  it 'should use fallback host when there is no config' do
    Rails.configuration.voxeo_configuration[:http_url_options] = {}
    Voxeo::UrlHelper.url('/', :host =>'foo.org').should eq('http://foo.org/')
  end

  it 'should build query string' do
    Voxeo::UrlHelper.url('/', {:key1 => 'value1', :key2 => 'value2'}).should eq('http://serverhost.com:1234/?key1=value1&key2=value2')
  end

end