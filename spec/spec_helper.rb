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

# This file is copied to spec/ when you run 'rails generate rspec:install'
ENV["RAILS_ENV"] = 'test'
require File.expand_path("../../config/environment", __FILE__)
require 'rspec/rails'
require 'devise'
require 'shoulda/matchers'

require 'webmock/rspec'
require 'listings/rspec'
require 'rspec/collection_matchers'

WebMock.disable_net_connect!(allow_localhost: true)

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

  # Render views in functional tests
  config.render_views

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

  config.infer_spec_type_from_file_location!

  # Exclude integration specs by default
  config.filter_run_excluding :integration => true unless config.filter_manager.inclusions[:integration]

  config.include AsteriskCall, integration: true

  config.before(:each) do
    Timecop.return
  end

  config.include Devise::TestHelpers, :type => :controller

  def expect_em_http(method, url, options = {})
    http = double('http')
    expect(EventMachine::HttpRequest).to receive(:new).with(url).and_return(http)

    http2 = double('http2')
    expect(http).to receive(method).with(options[:with]).and_return(http2)

    headers = double('headers', :status => 200)
    if options[:content_type]
      allow(headers).to receive(:[]).with('CONTENT_TYPE').and_return(options[:content_type])
    else
      allow(headers).to receive(:[]).with('CONTENT_TYPE')
    end
    allow(http2).to receive(:response_header).and_return(headers)

    expect(http2).to receive(:response).and_return(options[:and_return]) if options[:and_return]

    the_block = nil
    unless options[:callback] == false
      http2.define_singleton_method(:callback) do |&block|
        the_block = block
      end
    end
    expect(http2).to receive(:errback) unless options[:errback] == false

    if block_given?
      Fiber.new { yield }.resume
      the_block.call unless options[:callback] == false
    end
  end
end
