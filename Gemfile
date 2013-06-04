if RUBY_VERSION =~ /1.9/
  Encoding.default_external = Encoding::UTF_8
  Encoding.default_internal = Encoding::UTF_8
end

source 'http://rubygems.org'

gem 'rails', '3.2.11'

gem 'haml-rails'
gem "jquery-rails"

gem 'mysql2'
gem 'librevox'
gem 'batphone', :git => 'https://github.com/instedd/batphone', :branch => 'fastagi'
gem 'em-http-request'
gem 'eventmachine_httpserver', :require => 'evma_httpserver'
gem 'nokogiri'
gem 'guid'
gem 'devise'
gem "will_paginate", "~> 3.0"
gem "daemons"
gem 'rkelly', :git => 'https://github.com/manastech/rkelly.git'
gem "instedd-rails"
gem 'decent_exposure'
gem 'attr_encrypted'
gem 'foreman'
gem 'oauth2', :require => 'oauth2'
gem 'delayed_job_active_record'
gem 'rubyzip', :require => 'zip/zip'
gem 'rest-client'
gem 'enumerated_attribute', :git => "https://github.com/edave/enumerated_attribute.git"
gem 'csv_builder'
gem 'newrelic_rpm'
gem 'language_list'
gem 'formatted_rails_logger'

group :v8 do
  gem 'libv8', '~> 3.11.8'
end

group :assets do
  gem 'sass-rails',   '~> 3.2.5'
  gem 'coffee-rails', '~> 3.2.2'
  gem 'uglifier', '>= 1.0.3'
end

group :development, :test do
  gem 'machinist', git: 'https://github.com/tbuehl/machinist.git', branch: '1.0-maintenance'
  gem 'ffaker'
  gem 'mocha', :require => false
  gem 'rspec'
  gem 'rspec-rails'
  gem "shoulda-matchers"
  gem 'ci_reporter'
  gem 'equivalent-xml'
  gem 'pry-debugger'
  gem 'syntax'
  gem 'timecop'
end

group :development do
  gem 'capistrano'
  gem 'rvm'
  gem 'rvm-capistrano'
  gem 'licit'
end
