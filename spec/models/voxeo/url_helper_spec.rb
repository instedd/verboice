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