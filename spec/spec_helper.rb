# This file is copied to spec/ when you run 'rails generate rspec:install'
ENV["RAILS_ENV"] ||= 'test'
require File.expand_path("../../config/environment", __FILE__)
require File.expand_path(File.dirname(__FILE__) + '/blueprints')
require 'rspec/rails'
require 'rspec/autorun'

# Requires supporting ruby files with custom matchers and macros, etc,
# in spec/support/ and its subdirectories.
Dir[Rails.root.join("spec/support/**/*.rb")].each {|f| require f}

RSpec.configure do |config|
  # ## Mock Framework
  #
  # If you prefer to use mocha, flexmock or RR, uncomment the appropriate line:
  #
  # config.mock_with :mocha
  # config.mock_with :flexmock
  # config.mock_with :rr

  # Remove this line if you're not using ActiveRecord or ActiveRecord fixtures
  config.fixture_path = "#{::Rails.root}/spec/fixtures"

  # If you're not using ActiveRecord, or you'd prefer not to run each of your
  # examples within a transaction, remove the following line or assign false
  # instead of true.
  config.use_transactional_fixtures = true

  # If true, the base class of anonymous controllers will be inferred
  # automatically. This will be the default behavior in future versions of
  # rspec-rails.
  config.infer_base_class_for_anonymous_controllers = false

  def expect_em_http(method, url, options = {})
    http = mock('http')
    EventMachine::HttpRequest.should_receive(:new).with(url).and_return(http)

    http2 = mock('http2')
    http.should_receive(method).with(options[:with]).and_return(http2)

    headers = stub('headers', :status => 200)
    if options[:content_type]
      headers.stub(:[]).with('CONTENT_TYPE').and_return(options[:content_type])
    else
      headers.stub(:[]).with('CONTENT_TYPE')
    end
    http2.stub(:response_header).and_return(headers)

    http2.should_receive(:response).and_return(options[:and_return]) if options[:and_return]

    the_block = nil
    unless options[:callback] == false
      http2.define_singleton_method(:callback) do |&block|
        the_block = block
      end
    end
    http2.should_receive(:errback) unless options[:errback] == false

    if block_given?
      Fiber.new { yield }.resume
      the_block.call unless options[:callback] == false
    end
  end
end
