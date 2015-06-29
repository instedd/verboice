source 'http://rubygems.org'

gem 'rails', '~> 3.2.0'

gem 'haml-rails'
gem "jquery-rails"

gem 'mysql2'
gem 'nokogiri'
gem 'guid'
gem 'devise'
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
gem 'hub_client', github: 'instedd/ruby-hub_client', branch: 'master'
gem 'language_list'
gem 'bertrpc'
gem 'formatted_rails_logger'
gem 'instedd-pigeon', :require => 'pigeon'
gem 'poirot_rails', git: 'https://github.com/instedd/poirot_rails.git', branch: 'master'
gem 'hercule', git: 'https://github.com/instedd/poirot.git', branch: 'master'
gem 'alto_guisso', github: "instedd/alto_guisso", branch: 'master'
gem 'alto_guisso_rails', github: "instedd/alto_guisso_rails", branch: 'master'
gem 'ruby-openid'
gem 'omniauth'
gem 'omniauth-openid'
gem 'recurring_select', git: "https://github.com/instedd/recurring_select", branch: 'instedd'
gem 'listings'

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
  gem 'rvm-capistrano', require: false
  gem 'licit'
end

group :test do
  gem 'webmock'
  gem 'fakefs', :require => 'fakefs/safe'
end
