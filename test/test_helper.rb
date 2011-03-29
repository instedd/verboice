ENV["RAILS_ENV"] = "test"

require File.expand_path('../../config/environment', __FILE__)
require 'rails/test_help'
require File.expand_path(File.dirname(__FILE__) + '/blueprints')
require 'mocha'

class ActiveSupport::TestCase
  # Setup all fixtures in test/fixtures/*.(yml|csv) for all tests in alphabetical order.
  #
  # Note: You'll currently still have to declare fixtures explicitly in integration tests
  # -- they do not yet inherit this setting
  fixtures :all

  # Add more helper methods to be used by all tests here...
  include Mocha::API

  def expect_em_http(method, url, options = {})
    http = mock('http')
    EventMachine::HttpRequest.expects(:new).with(url).returns(http)

    http2 = mock('http2')
    http.expects(method).with(options[:with]).returns(http2)

    headers = stub('headers', :status => 200)
    http2.expects(:response_header).returns(headers)

    http2.expects(:response).returns(options[:returns])
    http2.expects(:callback).yields
    http2.expects(:errback)
  end

end
