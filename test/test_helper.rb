ENV["RAILS_ENV"] = "test"

require File.expand_path('../../config/environment', __FILE__)
require 'rails/test_help'
require File.expand_path(File.dirname(__FILE__) + '/blueprints')
require 'mocha'
require 'shoulda/rails'

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
    if options[:content_type]
      headers.stubs(:[]).with('CONTENT_TYPE').returns(options[:content_type])
    else
      headers.stubs(:[]).with('CONTENT_TYPE')
    end
    http2.stubs(:response_header).returns(headers)

    http2.expects(:response).returns(options[:returns]) if options[:returns]

    the_block = nil
    unless options[:callback] == false
      http2.define_singleton_method(:callback) do |&block|
        the_block = block
      end
    end
    http2.expects(:errback) unless options[:errback] == false

    if block_given?
      Fiber.new { yield }.resume
      the_block.call unless options[:callback] == false
    end
  end

end
